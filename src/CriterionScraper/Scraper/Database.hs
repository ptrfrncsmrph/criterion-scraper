{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module CriterionScraper.Scraper.Database
  ( allMovies,
    createDatabase,
    scrape,
    Movie (..),
    Movie' (..),
  )
where

import CriterionScraper.Prelude
import CriterionScraper.Scraper
  ( AppConfig (..),
    MonadDatabase (..),
  )
import qualified CriterionScraper.Scraper.API as API
import qualified CriterionScraper.Scraper.Movie as Scraper.Movie
import qualified Data.List as List
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.Types (Only (..))
import Servant (ServerError)
import Text.RawString.QQ (r)

data Movie = Movie
  { movieId :: UUID,
    created :: UTCTime,
    modified :: UTCTime,
    title :: Text,
    director :: Text,
    country :: Text,
    year :: Int
  }
  deriving (Show, ToJSON, Generic, FromRow)

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

moviesTable :: Query
moviesTable = "movies"

constraint :: Query
constraint = moviesTable <> "_title_director_year"

createDatabase :: (MonadReader AppConfig m, MonadDatabase m) => m Int64
createDatabase = do
  conn <- asks connection
  execute_ conn createMoviesTable

createMoviesTable :: Query
createMoviesTable =
  [r|CREATE TABLE IF NOT EXISTS |] <> moviesTable
    <> [r| 
    ( movie_id UUID PRIMARY KEY DEFAULT UUID_GENERATE_V4(),
      created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
      modified TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
      title TEXT NOT NULL,
      director TEXT NOT NULL,
      country TEXT NOT NULL,
      year INT NOT NULL,
      CONSTRAINT |]
    <> constraint
    <> [r| UNIQUE (title, director, year)
    ); 
  |]

insertMovie :: Query
insertMovie =
  [r|INSERT INTO |] <> moviesTable
    <> [r| (title, director, country, year)
    VALUES (?, ?, ?, ?)
    ON CONFLICT ON CONSTRAINT |]
    <> constraint
    <> [r|
    DO UPDATE
    SET modified = EXCLUDED.modified
    RETURNING *, (xmax = 0) AS inserted|]

allMovies :: (MonadDatabase m, MonadReader AppConfig m, MonadIO m) => m [Movie]
allMovies = do
  conn <- asks connection
  query_
    conn
    ("SELECT * from " <> moviesTable)

scrape :: (MonadDatabase m, MonadReader AppConfig m, MonadIO m, MonadError ServerError m) => m [Movie']
scrape = do
  putStrLn "Creating database"
  _n <- createDatabase
  putStrLn "Scraping movies"
  -- @TODO - Pete Murphy 2020-11-24 - Better way of doing this
  (movies :: [Scraper.Movie.Movie]) <- List.sortOn Scraper.Movie.year <$> API.scrapeAllMovies
  putStrLn "Success"
  conn <- asks connection
  (xs :: [(Movie, Only Bool)]) <-
    returningWith
      ((,) <$> fromRow @Movie <*> fromRow @(Only Bool))
      conn
      insertMovie
      (movies <&> \Scraper.Movie.Movie {..} -> (title, director, country, year))
  pure (xs <&> \(Movie {..}, Only wasInserted) -> Movie' {..})
