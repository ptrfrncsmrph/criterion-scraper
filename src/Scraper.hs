{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Scraper
  ( AppM (..),
    AppError (..),
    Config (..),
    MonadDatabase (..),
    MonadLogger (..),
  )
where

import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..))
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import Database.SQLite.Simple (Connection (..), Query, ToRow (..))
import qualified Database.SQLite.Simple as SQLite.Simple
import Prelude

newtype Config = Config {port :: String} deriving (Show)

newtype AppM a = AppM {runAppM :: ReaderT Config (ExceptT AppError IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO
    )

data AppError
  = ScrapeError

class MonadIO m => MonadDatabase m where
  execute :: ToRow q => Connection -> Query -> q -> m ()
  execute_ :: Connection -> Query -> m ()
  close :: Connection -> m ()
  open :: String -> m Connection

instance MonadDatabase AppM where
  execute = (fmap . fmap) liftIO . SQLite.Simple.execute
  execute_ = fmap liftIO . SQLite.Simple.execute_
  close = liftIO . SQLite.Simple.close
  open = liftIO . SQLite.Simple.open

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