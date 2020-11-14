{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..))
import Scraper
import Scraper.Database (run)
import Prelude

main :: IO ()
main = do
  config <- pure (Config {port = ""})
  runExceptT (runReaderT (runAppM run) config)
    >>= \case
      Left ScrapeError -> putStrLn "Failed while scraping"
      Right _ -> pure ()
