{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module CriterionScraper.Scraper.Movie
  ( Movie (..),
  )
where

import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Prelude

data Movie = Movie
  { mvTitle :: Text,
    mvDirector :: Text,
    mvCountry :: Text,
    mvYear :: Text
  }
  deriving (Show, Generic, ToRow, FromRow)