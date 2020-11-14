{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Scraper.API
  ( scrapeAllMovies,
  )
where

import Control.Monad.Except (ExceptT (ExceptT))
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Scraper (AppError (..), AppM)
import Scraper.Movie (Movie (..))
import Text.HTML.Scalpel ((@:), Config (..), Scraper)
import qualified Text.HTML.Scalpel as Scalpel

scrapeAllMovies :: AppM [Movie]
scrapeAllMovies =
  ExceptT
    (note ScrapeError <$> Scalpel.scrapeURLWithConfig config "https://films.criterionchannel.com" movieRoot)
  where
    config =
      Config {decoder = Scalpel.utf8Decoder, manager = Nothing}
    movieRoot :: Scraper Text [Movie]
    movieRoot =
      Scalpel.chroots "tr" movie
    cleanupText :: Text -> Text
    cleanupText =
      Text.dropAround ((== '…') ||| (== ',') ||| Char.isSpace)
    movie :: Scraper Text Movie
    movie = do
      mvTitle <-
        cleanupText
          <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--title"])
      mvDirector <-
        cleanupText
          <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--director"])
      mvCountry <-
        cleanupText
          <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--country"])
      mvYear <-
        cleanupText
          <$> Scalpel.text ("td" @: [Scalpel.hasClass "criterion-channel__td--year"])
      pure
        Movie
          { mvTitle,
            mvDirector,
            mvCountry,
            mvYear
          }

(|||) :: Applicative f => f Bool -> f Bool -> f Bool
p ||| p' = (||) <$> p <*> p'

note :: a -> Maybe b -> Either a b
note _ (Just x) = Right x
note e Nothing = Left e
