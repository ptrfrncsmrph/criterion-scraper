{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module CriterionScraper.Scraper.Movie
  ( ScrapedMovie (..),
  )
where

import CriterionScraper.Prelude
import Database.PostgreSQL.Simple (FromRow, ToRow)

-- | The scraped movie
data ScrapedMovie = ScrapedMovie
  { title :: Text,
    director :: Text,
    country :: Text,
    year :: Int,
    thumbnailURL :: Text,
    detailsURL :: Text
  }
  deriving (Show, Generic, ToRow, FromRow)