{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module CriterionScraper.Scraper
  ( AppConfig (..),
    AppEnvironment (..),
    AppError (..),
    AppM (..),
    MonadDatabase (..),
    MonadLogger (..),
    parseEnv,
  )
where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Reader.Class (MonadReader)
import CriterionScraper.Lib (note)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, Query, ToRow)
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import Text.Read (readMaybe)
import Prelude

newtype AppM a = AppM {runAppM :: ReaderT AppConfig (ExceptT AppError IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppConfig
    )

data AppConfig = AppConfig
  { environment :: AppEnvironment,
    connection :: Connection
  }

data AppEnvironment = Production | Development

-- @TODO: Use typed errors
newtype AppError = AppError String
  deriving (Show)

instance Exception AppError

class MonadIO m => MonadDatabase m where
  execute :: ToRow q => Connection -> Query -> q -> m Int64
  execute_ :: Connection -> Query -> m Int64
  close :: Connection -> m ()

instance MonadDatabase AppM where
  execute = (fmap . fmap) liftIO . PostgreSQL.Simple.execute
  execute_ = fmap liftIO . PostgreSQL.Simple.execute_
  close = liftIO . PostgreSQL.Simple.close

instance MonadError AppError AppM where
  throwError = AppM . ReaderT . pure . ExceptT . pure . Left
  catchError (AppM (ReaderT f)) handler = AppM $
    ReaderT \conn ->
      ExceptT $
        runExceptT (f conn) >>= \case
          Left x ->
            let f' = runReaderT (runAppM (handler x))
             in runExceptT (f' conn)
          Right x -> pure (Right x)

class MonadIO m => MonadLogger m where
  log :: Text -> m ()

instance MonadLogger AppM where
  log = liftIO . Text.IO.putStrLn

parseEnv :: [(String, String)] -> Either AppError ConnectInfo
parseEnv env = do
  connectHost <-
    lookup "CONNECT_HOST" env
      & noteAppError "Missing host"
  connectPort <- do
    port <-
      lookup "CONNECT_PORT" env
        & noteAppError "Missing port"
    readMaybe port
      & noteAppError ("Failed to parse port as Word16: " <> port)
  connectUser <-
    lookup "CONNECT_USER" env
      & noteAppError "Missing user"
  connectPassword <-
    lookup "CONNECT_PASSWORD" env
      & noteAppError "Missing password"
  connectDatabase <-
    lookup "CONNECT_DATABASE" env
      & noteAppError "Missing database"
  pure
    ConnectInfo
      { connectHost,
        connectPort,
        connectUser,
        connectPassword,
        connectDatabase
      }
  where
    noteAppError = note . AppError
