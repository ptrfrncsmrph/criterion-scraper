{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module CriterionScraper.API
  ( API,
    api,
  )
where

import CriterionScraper.Prelude
import CriterionScraper.Scraper.Database
  ( Movie (..),
    Movie' (..),
  )
import Servant

type API =
  "movies" :> Get '[JSON] [Movie]
    :<|> "scrape" :> Post '[JSON] [Movie']

api :: Proxy API
api = Proxy
