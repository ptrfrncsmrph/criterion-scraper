{-# LANGUAGE RecordWildCards #-}

module Scraper.Movie
  ( Movie (..),
  )
where

import Data.Text (Text)
import Database.SQLite.Simple (ToRow (..))

data Movie
  = Movie
      { mvTitle :: Text,
        mvDirector :: Text,
        mvCountry :: Text,
        mvYear :: Text
      }
  deriving (Show)

instance ToRow Movie where
  toRow Movie {..} = toRow (mvTitle, mvDirector, mvCountry, mvYear)
