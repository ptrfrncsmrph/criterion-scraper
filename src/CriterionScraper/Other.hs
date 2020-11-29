module CriterionScraper.Other where

import qualified Control.Exception as Exception
import Control.Monad.Except
  ( liftEither,
    runExceptT,
    throwError,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans.Except (ExceptT)
import CriterionScraper.API
import Database.PostgreSQL.Simple (Connection (..))
import qualified Database.PostgreSQL.Simple as PostgreSQL
import Servant
import Prelude

newtype App a = App {runApp :: ReaderT Connection IO a}

newtype App' a = App' {runApp' :: ExceptT ServerError (ReaderT Connection IO) a}

app :: App ()
app = undefined

app' :: App' ()
app' = undefined

main :: IO ()
main = do
  connection <- PostgreSQL.connect PostgreSQL.defaultConnectInfo
  runReaderT (runApp app) connection

main2 :: IO ()
main2 =
  Exception.bracket
    (PostgreSQL.connect PostgreSQL.defaultConnectInfo)
    (PostgreSQL.close)
    (runReaderT (runApp app))

main' :: IO ()
main' =
  Exception.bracket
    (PostgreSQL.connect PostgreSQL.defaultConnectInfo)
    (PostgreSQL.close)
    (runReaderT (runExceptT (runApp' app') >>= either (liftIO . print) pure))

main'' :: IO ()
main'' =
  Exception.bracket
    (PostgreSQL.connect PostgreSQL.defaultConnectInfo)
    (PostgreSQL.close)
    (runReaderT (runExceptT (runApp' app') >>= either (liftIO . print) pure))

foo :: Server API
foo = undefined