{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Scraper.Database
  ( run,
    getAllMovies,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Database.SQLite.Simple (Connection, Query)
import qualified Database.SQLite.Simple as SQLite.Simple
import Database.SQLite.Simple.Types (Null (..))
import Scraper (AppM)
import qualified Scraper.API as API
import Scraper.Movie (Movie (..))
import Text.RawString.QQ (r)

createDatabase :: IO Connection
createDatabase = do
  conn <- SQLite.Simple.open "movies.db"
  SQLite.Simple.execute_ conn createMoviesTable
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

run :: AppM ()
run = do
  conn <- liftIO createDatabase
  movies <- List.sortOn mvYear <$> API.scrapeAllMovies
  liftIO do
    Foldable.for_
      movies
      \Movie {..} -> do
        SQLite.Simple.execute conn insertMovie (Null, mvTitle, mvDirector, mvCountry, mvYear)
        Text.IO.putStrLn do
          "Inserting: "
            <> Text.intercalate
              ", "
              [ mvYear,
                mvTitle,
                mvDirector,
                mvCountry
              ]
  liftIO (SQLite.Simple.close conn)
