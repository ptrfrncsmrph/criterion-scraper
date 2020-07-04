{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable (forM_, traverse_)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T.IO
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Lib

main :: IO ()
main = do
  conn <- createDatabase
  wat <- allMovies
  case wat of
    Nothing -> putStrLn "No Comments!"
    Just cs -> do
      putStrLn "Gonna print-a the lines!"
      forM_ (processMovies cs) \Movie {..} -> do
        execute conn insertMovie (Null, mvTitle, mvDirector, mvCountry, mvYear)
        T.IO.putStrLn do
          "Inserting: "
            <> T.intercalate
              ", "
              [ mvYear,
                mvTitle,
                mvDirector,
                mvCountry
              ]
  close conn
