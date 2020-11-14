module Scraper
  ( AppM,
    AppError (..),
  )
where

import Control.Monad.Except (ExceptT)

type AppM = ExceptT AppError IO

data AppError
  = ScrapeError
