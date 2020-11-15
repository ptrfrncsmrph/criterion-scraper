{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Scraper.API
  ( scrapeAllMovies,
  )
where

import Control.Monad.Error.Class (MonadError (..), liftEither)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Lib (note, (|||))
import Scraper (AppError (..))
import Scraper.Movie (Movie (..))
import Text.HTML.Scalpel (Config (..), Scraper, (@:))
import qualified Text.HTML.Scalpel as Scalpel
import Prelude hiding (log)

scrapeAllMovies :: (MonadError AppError m, MonadIO m) => m [Movie]
scrapeAllMovies =
  liftEither
    =<< liftIO do
      note (AppError "Error while scraping")
        <$> Scalpel.scrapeURLWithConfig
          config
          "https://films.criterionchannel.com"
          movieRoot
  where
    config =
      Config {decoder = Scalpel.utf8Decoder, manager = Nothing}
    movieRoot =
      Scalpel.chroots "tr" movie
    cleanupText =
      Text.dropAround ((== '…') ||| (== ',') ||| Char.isSpace)
    movie :: Scraper Text Movie
    movie = do
      mvTitle <-
        cleanupText <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--title"])
      mvDirector <-
        cleanupText <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--director"])
      mvCountry <-
        cleanupText <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--country"])
      mvYear <-
        cleanupText <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--year"])
      pure
        Movie
          { mvTitle,
            mvDirector,
            mvCountry,
            mvYear
          }
