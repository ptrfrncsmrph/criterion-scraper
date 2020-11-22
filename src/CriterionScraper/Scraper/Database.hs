{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module CriterionScraper.Scraper.Database
  ( runScraper,
    getAllMovies,
    createDatabase,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask, asks)
import CriterionScraper.Scraper (AppConfig (..), AppError, MonadDatabase (..), MonadLogger (..))
import qualified CriterionScraper.Scraper.API as API
import CriterionScraper.Scraper.Movie (Movie (..))
import qualified Data.Foldable as Foldable
import Data.Int (Int64)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, Query)
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Null (..))
import qualified System.Environment as Environment
import Text.RawString.QQ (r)
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (log)

moviesTable = "movies_2020_11_21"

constraint = moviesTable <> "title_director_year"

createDatabase :: (MonadReader AppConfig m, MonadError AppError m, MonadDatabase m) => m Int64
createDatabase = do
  conn <- asks connection
  execute_ conn createMoviesTable

createMoviesTable :: Query
createMoviesTable =
  [r|CREATE TABLE IF NOT EXISTS |] <> moviesTable
    <> [r| 
    ( id SERIAL PRIMARY KEY,
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
    <> [r| (title, director, country, year) VALUES (?, ?, ?, ?)
    ON CONFLICT ON CONSTRAINT |]
    <> constraint
    <> [r| DO NOTHING |]

getAllMovies :: Query
getAllMovies =
  unsafeCoerce do
    "SELECT * from " <> moviesTable

runScraper :: (MonadDatabase m, MonadReader AppConfig m, MonadIO m, MonadError AppError m, MonadLogger m) => m ()
runScraper = do
  log "Creating database"
  _n <- createDatabase
  log "Scraping movies"
  movies <- List.sortOn mvYear <$> API.scrapeAllMovies
  log "Success"
  conn <- asks connection
  Foldable.for_
    movies
    \Movie {..} -> do
      let row = Text.intercalate ", " [mvYear, mvTitle, mvDirector, mvCountry]
      execute
        conn
        insertMovie
        (mvTitle, mvDirector, mvCountry, read @Int (Text.unpack mvYear))
        >>= \case
          0 -> log ("Skipping: " <> row)
          _ -> log ("Inserting: " <> row)
  close conn
