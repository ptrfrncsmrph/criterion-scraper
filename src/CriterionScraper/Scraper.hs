{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module CriterionScraper.Scraper
  ( AppConfig (..),
    AppEnvironment (..),
    AppM (..),
    MonadDatabase (..),
  )
where

import CriterionScraper.Prelude
import Database.PostgreSQL.Simple
  ( Connection,
    FromRow,
    Query,
    ToRow,
  )
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (RowParser)
import Servant (ServerError (..))

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

data AppEnvironment = Production | Development deriving (Show, Eq)

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
