{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module CriterionScraper.Scraper
  ( AppConfig (..),
    AppEnvironment (..),
    AppM (..),
    MonadDatabase (..),
    parseEnv,
  )
where

import CriterionScraper.Prelude
import qualified Data.List as List
import Database.PostgreSQL.Simple
  ( ConnectInfo (..),
    Connection,
    FromRow,
    Query,
    ToRow,
  )
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (RowParser)
import Servant (ServerError (..), err500)

newtype AppM a = AppM {runAppM :: ReaderT AppConfig (ExceptT ServerError IO) a}
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

class MonadIO m => MonadDatabase m where
  execute :: ToRow q => Connection -> Query -> q -> m Int64
  execute_ :: Connection -> Query -> m Int64
  query :: (ToRow q, FromRow r) => Connection -> Query -> q -> m [r]
  query_ :: FromRow r => Connection -> Query -> m [r]
  returning :: (ToRow q, FromRow r) => Connection -> Query -> [q] -> m [r]
  returningWith :: ToRow q => RowParser r -> Connection -> Query -> [q] -> m [r]

instance MonadDatabase AppM where
  execute = (fmap . fmap) liftIO . PostgreSQL.Simple.execute
  execute_ = fmap liftIO . PostgreSQL.Simple.execute_
  query = (fmap . fmap) liftIO . PostgreSQL.Simple.query
  query_ = fmap liftIO . PostgreSQL.Simple.query_
  returning = (fmap . fmap) liftIO . PostgreSQL.Simple.returning
  returningWith = (fmap . fmap . fmap) liftIO . PostgreSQL.Simple.returningWith

instance MonadError ServerError AppM where
  throwError = AppM . ReaderT . pure . ExceptT . pure . Left
  catchError (AppM (ReaderT f)) handler = AppM do
    ReaderT \conn ->
      ExceptT
        ( runExceptT (f conn) >>= \case
            Left x ->
              let f' = runReaderT (runAppM (handler x))
               in runExceptT (f' conn)
            Right x -> pure (Right x)
        )

parseEnv :: [(String, String)] -> Either ServerError ConnectInfo
parseEnv env = do
  connectHost <-
    List.lookup "CONNECT_HOST" env
      & noteAppError "Missing host"
  connectPort <- do
    port <-
      List.lookup "CONNECT_PORT" env
        & noteAppError "Missing port"
    readMaybe port
      & noteAppError ("Failed to parse port as Word16: " <> port)
  connectUser <-
    List.lookup "CONNECT_USER" env
      & noteAppError "Missing user"
  connectPassword <-
    List.lookup "CONNECT_PASSWORD" env
      & noteAppError "Missing password"
  connectDatabase <-
    List.lookup "CONNECT_DATABASE" env
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
    noteAppError errReasonPhrase = note (err500 {errReasonPhrase})
