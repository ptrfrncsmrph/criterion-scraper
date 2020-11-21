module CriterionScraper.Main where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Control.Applicative (Applicative (liftA2))
import Control.Monad.Except (liftEither, runExceptT, throwError)
import Control.Monad.Reader (ReaderT (..))
import CriterionScraper.Scraper (AppError (..))
import qualified CriterionScraper.Scraper as Scraper
import CriterionScraper.Scraper.Database (run)
import Prelude

main :: IO ()
main =
  runExceptT do
    env <- loadFile defaultConfig `onMissingFile` throwError (AppError "Failed to load .env")
    connectInfo <- liftEither (Scraper.parseEnv env)
    runReaderT (Scraper.runAppM run) connectInfo
    >>= either print mempty
