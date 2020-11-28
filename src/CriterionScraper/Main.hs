module CriterionScraper.Main (main) where

import Configuration.Dotenv
  ( defaultConfig,
    loadFile,
    onMissingFile,
  )
import qualified Control.Exception as Exception
import CriterionScraper.Prelude
import CriterionScraper.Scraper
  ( AppConfig (..),
    AppEnvironment (..),
    AppError (..),
    AppM (..),
  )
import qualified CriterionScraper.Scraper as Scraper
import CriterionScraper.Scraper.Database (scrape)
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple

main :: IO ()
main =
  runExceptT do
    connectionInfo <-
      liftEither . Scraper.parseEnv
        =<< loadFile defaultConfig `onMissingFile` throwError (AppError "Failed to load .env")
    liftIO do
      Exception.bracket
        (PostgreSQL.Simple.connect connectionInfo)
        (PostgreSQL.Simple.close)
        \connection -> runExceptT do
          runReaderT (runAppM app) (AppConfig {connection, environment = Development})
    >>= either print mempty

app :: AppM ()
app = error "unimplemented"