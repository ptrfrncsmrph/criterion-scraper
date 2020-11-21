{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module CriterionScraper.Scraper.Database
  ( run,
    getAllMovies,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import CriterionScraper.Scraper (AppError, MonadDatabase (..), MonadLogger (..))
import qualified CriterionScraper.Scraper.API as API
import CriterionScraper.Scraper.Movie (Movie (..))
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text
import Database.PostgreSQL.Simple (ConnectInfo, Connection, Query)
import Database.PostgreSQL.Simple.Types (Null (..))
import Text.RawString.QQ (r)
import Prelude hiding (log)

createDatabase :: (MonadError AppError m, MonadReader ConnectInfo m, MonadDatabase m) => m Connection
createDatabase = do
  connectInfo <- ask
  conn <- open connectInfo
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

run :: (MonadDatabase m, MonadReader ConnectInfo m, MonadIO m, MonadError AppError m, MonadLogger m) => m ()
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
        (mvTitle, mvDirector, mvCountry, read @Int (Text.unpack mvYear))
      log ("Inserting: " <> Text.intercalate ", " [mvYear, mvTitle, mvDirector, mvCountry])
  close conn
