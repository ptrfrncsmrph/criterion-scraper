module CriterionScraper.Main (main) where

import Configuration.Dotenv
  ( defaultConfig,
    loadFile,
    onMissingFile,
  )
import qualified Control.Exception as Exception
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
    env <-
      liftEither . Scraper.parseEnv
        =<< loadFile defaultConfig `onMissingFile` throwError (AppError "Failed to load .env")
    liftIO do
      Exception.bracket
        (PostgreSQL.Simple.connect env)
        (PostgreSQL.Simple.close)
        \connection -> runExceptT do
          runReaderT (Scraper.runAppM runScraper) (AppConfig {connection, environment = Development})
    >>= either print mempty
