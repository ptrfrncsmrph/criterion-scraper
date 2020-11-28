{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module CriterionScraper.Scraper.Database
  ( runScraper,
    getAllMovies,
    createDatabase,
    Movie (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask, asks)
import CriterionScraper.Scraper
  ( AppConfig (..),
    AppError,
    MonadDatabase (..),
    MonadLogger (..),
  )
import qualified CriterionScraper.Scraper.API as API
import qualified CriterionScraper.Scraper.Movie as Scraper.Movie
import qualified Data.Foldable as Foldable
import Data.Functor ((<&>))
import Data.Int (Int64)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, Query)
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow (..), RowParser)
import Database.PostgreSQL.Simple.Types (Null (..), Only (..))
import GHC.Generics (Generic)
import Text.RawString.QQ (r)
import Prelude hiding (log)

data Movie = Movie
  { movieId :: UUID,
    created :: UTCTime,
    modified :: UTCTime,
    title :: Text,
    director :: Text,
    country :: Text,
    year :: Int
  }
  deriving (Show, Generic, FromRow)

moviesTable :: Query
moviesTable = "movies_2020_11_21"

constraint :: Query
constraint = moviesTable <> "_title_director_year"

createDatabase :: (MonadReader AppConfig m, MonadError AppError m, MonadDatabase m) => m Int64
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

getAllMovies :: Query
getAllMovies =
  "SELECT * from " <> moviesTable

runScraper :: (MonadDatabase m, MonadReader AppConfig m, MonadIO m, MonadError AppError m, MonadLogger m) => m ()
runScraper = do
  log "Creating database"
  _n <- createDatabase
  log "Scraping movies"
  -- @TODO - Pete Murphy 2020-11-24 - Better way of doing this
  (movies :: [Scraper.Movie.Movie]) <- List.sortOn Scraper.Movie.year <$> API.scrapeAllMovies
  log "Success"
  conn <- asks connection
  (xs :: [(Movie, Only Bool)]) <-
    returningWith
      ((,) <$> fromRow @Movie <*> fromRow @(Only Bool))
      conn
      insertMovie
      (movies <&> \Scraper.Movie.Movie {..} -> (title, director, country, year))
  log (Text.pack (show xs))
