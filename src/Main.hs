module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.Reader (ReaderT (..))
import Scraper (AppError (..))
import qualified Scraper as Scraper
import Scraper.Database (run)
import Prelude

main :: IO ()
main = do
  env <- loadFile defaultConfig
  runExceptT do
    connectionInfo <- liftEither (Scraper.parseEnv env)
    pure (runReaderT (Scraper.runAppM run) connectionInfo)
    >>= \case
      Left (AppError str) -> putStrLn str
      Right _ -> pure ()
