{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module CriterionScraper.API where

import CriterionScraper.Prelude
import CriterionScraper.Scraper.Database (Movie (..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant

type DBConnectionString = ByteString

type ScraperAPI =
  "movies" :> Get '[JSON] [Movie]
    :<|> "scrape" :> Post '[JSON] [Movie']

data Movie' = Movie'
  { movieId :: UUID,
    created :: UTCTime,
    modified :: UTCTime,
    title :: Text,
    director :: Text,
    country :: Text,
    year :: Int,
    wasInserted :: Bool
  }
  deriving (Generic, ToJSON, Show)

scraperAPI :: Proxy ScraperAPI
scraperAPI = Proxy

initDB :: DBConnectionString -> IO ()
initDB connStr = error "unimplemented"
