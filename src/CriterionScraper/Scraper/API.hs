{-# LANGUAGE ApplicativeDo #-}

module CriterionScraper.Scraper.API
  ( scrapeAllMovies,
  )
where

import CriterionScraper.Prelude
-- import CriterionScraper.Scraper (AppError (..))
import CriterionScraper.Scraper.Movie (Movie (..))
import qualified Data.Char as Char
import qualified Data.Text as Text
import Servant (ServerError (..), err500)
import Text.HTML.Scalpel (Config (..), Scraper, (@:))
import qualified Text.HTML.Scalpel as Scalpel

scrapeAllMovies :: (MonadError ServerError m, MonadIO m) => m [Movie]
scrapeAllMovies =
  liftEither
    =<< liftIO do
      note (err500 {errBody = "Error while scraping"})
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
    (|||) = liftA2 (||)
    movie :: Scraper Text Movie
    movie = do
      title <-
        cleanupText <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--title"])
      director <-
        cleanupText <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--director"])
      country <-
        cleanupText <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--country"])
      year <-
        unsafeRead . Text.unpack . cleanupText
          <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--year"])
      pure
        Movie
          { title,
            director,
            country,
            year
          }
