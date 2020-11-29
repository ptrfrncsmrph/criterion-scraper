module CriterionScraper.Main (main) where

import qualified Configuration.Dotenv as Dotenv
import qualified Control.Exception as Exception
import CriterionScraper.API (API, api)
import CriterionScraper.Prelude
import CriterionScraper.Scraper
  ( AppConfig (..),
    AppEnvironment (..),
    AppM (..),
  )
import qualified CriterionScraper.Scraper as Scraper
import CriterionScraper.Scraper.Database (allMovies, scrape)
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import qualified Network.Wai.Handler.Warp as Warp
import Servant
  ( Handler (..),
    Server,
    ServerT,
    (:<|>) (..),
  )
import qualified Servant

main :: IO ()
main =
  runExceptT do
    connectionInfo <-
      liftEither . Scraper.parseEnv
        =<< Dotenv.loadFile Dotenv.defaultConfig
          `Dotenv.onMissingFile` throwError Servant.err500
    liftIO do
      Exception.bracket
        (PostgreSQL.Simple.connect connectionInfo)
        PostgreSQL.Simple.close
        \connection -> runExceptT do
          let cfg = AppConfig {connection, environment = Development}
          runReaderT (runAppM (liftIO $ Warp.run 8080 (Servant.serve api (appToServer cfg)))) cfg
    -- @TODO :(
    >>= either print (either print mempty)

server :: ServerT API AppM
server = allMovies :<|> scrape

appToServer :: AppConfig -> Server API
appToServer cfg = Servant.hoistServer api (convertApp cfg) server

convertApp :: AppConfig -> AppM a -> Handler a
convertApp cfg (AppM a) = Handler (runReaderT a cfg)