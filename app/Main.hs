{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable (forM_, traverse_)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T.IO
import Lib

main :: IO ()
main = do
  wat <- allMovies
  case wat of
    -- Nothing -> T.IO.putStrLn "No Comments!"
    Nothing -> putStrLn "No Comments!"
    Just cs -> do
      -- T.IO.putStrLn "Gonna print-a the lines!"
      putStrLn "Gonna print-a the lines!"
      forM_ (processMovies cs) \Movie {..} ->
        -- T.IO.putStrLn
        T.IO.putStrLn
          ( T.intercalate
              "\t"
              [ mvYear,
                mvTitle,
                mvDirector
              ]
          )
