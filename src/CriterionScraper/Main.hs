module CriterionScraper.Main (main) where

import qualified Control.Exception as Exception
import CriterionScraper.API (API, api)
import CriterionScraper.Prelude
import CriterionScraper.Scraper
  ( AppConfig (..),
    AppEnvironment (..),
    AppM (..),
  )
import CriterionScraper.Scraper.Database (allMovies, scrape)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import qualified Network.Wai.Handler.Warp as Warp
import Servant
  ( Handler (..),
    Server,
    ServerT,
    (:<|>) (..),
  )
import qualified Servant
import qualified System.Environment as Environment
import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  environment <-
    Environment.lookupEnv "ENVIRONMENT" <&> \case
      (fmap (map Char.toLower) -> Just "production") -> Production
      _ -> Development
  port <-
    Environment.lookupEnv "PORT" <&> \case
      (fmap unsafeRead -> Just n) -> n
      _ -> 8080
  connectionString <-
    (fmap ByteString.pack <$> Environment.lookupEnv "DATABASE_URL")
      `whenNothingM` if environment == Production
        then error "Failed to find DATABASE_URL"
        else pure (PostgreSQL.Simple.postgreSQLConnectionString PostgreSQL.Simple.defaultConnectInfo)

  -- @TODO - Pete Murphy 2020-11-29 - Temporary
  putStrLn ("Using environment: " <> show environment)
  putStrLn ("Using port: " <> show port)
  putStrLn ("Using connectionString: " <> show connectionString)

  _ <- Exception.bracket
    (PostgreSQL.Simple.connectPostgreSQL connectionString)
    PostgreSQL.Simple.close
    \connection -> runExceptT do
      let cfg = AppConfig {connection, environment}
          serveApp = Warp.run port (Servant.serve api (appToServer cfg))
      runReaderT (runAppM (liftIO serveApp)) cfg
  pure ()

appToServer :: AppConfig -> Server API
appToServer cfg = Servant.hoistServer api (convertApp cfg) server

convertApp :: AppConfig -> AppM a -> Handler a
convertApp cfg (AppM a) = Handler (runReaderT a cfg)

server :: ServerT API AppM
server = allMovies :<|> scrape