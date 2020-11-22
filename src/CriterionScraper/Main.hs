module CriterionScraper.Main where

import Configuration.Dotenv
  ( defaultConfig,
    loadFile,
    onMissingFile,
  )
import Control.Applicative (Applicative (liftA2))
import Control.Monad.Except
  ( liftEither,
    runExceptT,
    throwError,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (..))
import CriterionScraper.Scraper
  ( AppConfig (..),
    AppEnvironment (..),
    AppError (..),
  )
import qualified CriterionScraper.Scraper as Scraper
import CriterionScraper.Scraper.Database (runScraper)
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import Prelude

main :: IO ()
main =
  runExceptT do
    env <- loadFile defaultConfig `onMissingFile` throwError (AppError "Failed to load .env")
    connection <- liftIO . PostgreSQL.Simple.connect =<< liftEither (Scraper.parseEnv env)
    runReaderT (Scraper.runAppM runScraper) (AppConfig {connection, environment = Development})
    >>= either print mempty
