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
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, Query)
import qualified Database.PostgreSQL.Simple as PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Null (..))
import qualified System.Environment as Environment
import Text.RawString.QQ (r)
import Prelude hiding (log)

createDatabase :: (MonadReader AppConfig m, MonadError AppError m, MonadDatabase m) => m Connection
createDatabase = do
  conn <- asks connection
  execute_ conn createMoviesTable
  pure conn

createMoviesTable :: Query
createMoviesTable =
  [r|
    CREATE TABLE IF NOT EXISTS movies
    ( id SERIAL PRIMARY KEY,
      title TEXT NOT NULL,
      director TEXT NOT NULL,
      country TEXT NOT NULL,
      year INT NOT NULL
    )
  |]

insertMovie :: Query
insertMovie =
  "INSERT INTO movies (title, director, country, year) VALUES (?, ?, ?, ?)"

getAllMovies :: Query
getAllMovies =
  "SELECT * from movies"

runScraper :: (MonadDatabase m, MonadReader AppConfig m, MonadIO m, MonadError AppError m, MonadLogger m) => m ()
runScraper = do
  conn <- asks connection
  log "Scraping movies"
  movies <- List.sortOn mvYear <$> API.scrapeAllMovies
  log "Success"
  Foldable.for_
    movies
    \Movie {..} -> do
      execute
        conn
        insertMovie
        (mvTitle, mvDirector, mvCountry, read @Int (Text.unpack mvYear))
      log ("Inserting: " <> Text.intercalate ", " [mvYear, mvTitle, mvDirector, mvCountry])
  close conn
