{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module CriterionScraper.API where

import CriterionScraper.Prelude
import CriterionScraper.Scraper.Database
  ( Movie (..),
    Movie' (..),
    allMovies,
    scrape,
  )
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant

type API =
  "movies" :> Get '[JSON] [Movie]
    :<|> "scrape" :> Post '[JSON] [Movie']

api :: Proxy API
api = Proxy

-- server :: Server API
-- server = (toHandler allMovies) :<|> (toHandler scrape)

toHandler = error ""
