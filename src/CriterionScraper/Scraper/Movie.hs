{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module CriterionScraper.Scraper.Movie
  ( Movie (..),
  )
where

import CriterionScraper.Prelude
import Database.PostgreSQL.Simple (FromRow, ToRow)

data Movie = Movie
  { title :: Text,
    director :: Text,
    country :: Text,
    year :: Int
  }
  deriving (Show, Generic, ToRow, FromRow)