{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Char
import Data.List
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import GHC.Generics
import Text.HTML.Scalpel
import Text.RawString.QQ

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

allMovies :: IO (Maybe [Movie])
allMovies = scrapeURLWithConfig config "https://films.criterionchannel.com" movieRoot
  where
    config = Config {decoder = utf8Decoder, manager = Nothing}
    movieRoot :: Scraper Text [Movie]
    movieRoot = chroots "tr" movie
    processText :: Text -> Text
    processText = T.dropAround ((== '…') ||| (== ',') ||| isSpace)
    movie :: Scraper Text Movie
    movie = do
      t <- processText <$> text ("td" @: [hasClass "criterion-channel__td--title"])
      d <- processText <$> text ("td" @: [hasClass "criterion-channel__td--director"])
      c <- processText <$> text ("td" @: [hasClass "criterion-channel__td--country"])
      y <- processText <$> text ("td" @: [hasClass "criterion-channel__td--year"])
      pure
        Movie
          { mvTitle = t,
            mvDirector = d,
            mvCountry = c,
            mvYear = y
          }

p ||| p' = (||) <$> p <*> p'

processMovies :: [Movie] -> [Movie]
processMovies = sortOn mvYear

-- ************* Database stuff *************

createDatabase :: IO Connection
createDatabase = do
  conn <- open "movies.db"
  execute_ conn createMoviesTable
  pure conn

createMoviesTable :: Query
createMoviesTable =
  [r|
    CREATE TABLE IF NOT EXISTS movies
    ( id INTEGER PRIMARY KEY AUTOINCREMENT,
      title TEXT,
      director TEXT,
      country TEXT,
      year TEXT
    )
  |]

insertMovie :: Query
insertMovie =
  "INSERT INTO movies VALUES (?, ?, ?, ?, ?)"

getAllMovies :: Query
getAllMovies =
  "SELECT * from movies"
