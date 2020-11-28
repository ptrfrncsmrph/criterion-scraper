module CriterionScraper.Main (main) where

import qualified Configuration.Dotenv as Dotenv
import qualified Control.Exception as Exception
import CriterionScraper.Prelude
import CriterionScraper.Scraper
  ( AppConfig (..),
    AppEnvironment (..),
    AppM (..),
  )
import qualified CriterionScraper.Scraper as Scraper
import CriterionScraper.Scraper.Database (scrape)
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import Servant.Server (err500)

main :: IO ()
main =
  do
    runExceptT do
      connectionInfo <-
        liftEither . Scraper.parseEnv
          =<< Dotenv.loadFile Dotenv.defaultConfig
            `Dotenv.onMissingFile` throwError err500
      liftIO do
        Exception.bracket
          (PostgreSQL.Simple.connect connectionInfo)
          (PostgreSQL.Simple.close)
          \connection -> runExceptT do
            runReaderT (runAppM scrape) (AppConfig {connection, environment = Development})
    >>= either print mempty

app :: AppM ()
app = error "unimplemented"

-- server :: Server API
-- server = allMovies :<|> scrape