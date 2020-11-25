{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module CriterionScraper.API where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import CriterionScraper.Scraper.Database (Movie (..))
import Data.Aeson (ToJSON (..))
import Data.ByteString (ByteString)
import qualified Data.Monoid as Monoid
import qualified Data.Pool as Pool
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.API
import Prelude

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
initDB connStr = Exception.bracket undefined undefined undefined
