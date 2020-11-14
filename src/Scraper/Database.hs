{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Scraper.Database
  ( run,
    getAllMovies,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, Query)
import Database.SQLite.Simple.Types (Null (..))
import Scraper (AppError, MonadDatabase (..), MonadLogger (..))
import qualified Scraper.API as API
import Scraper.Movie (Movie (..))
import Text.RawString.QQ (r)
import Prelude hiding (log)

createDatabase :: MonadDatabase m => m Connection
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

run :: (MonadDatabase m, MonadIO m, MonadError AppError m, MonadLogger m) => m ()
run = do
  log "Creating database"
  conn <- createDatabase
  log "Scraping movies"
  movies <- List.sortOn mvYear <$> API.scrapeAllMovies
  log "Success"
  Foldable.for_
    movies
    \Movie {..} -> do
      execute
        conn
        insertMovie
        (Null, mvTitle, mvDirector, mvCountry, mvYear)
      log ("Inserting: " <> Text.intercalate ", " [mvYear, mvTitle, mvDirector, mvCountry])
  close conn
