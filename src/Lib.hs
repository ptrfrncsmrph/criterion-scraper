module Lib (note, (|||)) where

import Prelude

(|||) :: Applicative f => f Bool -> f Bool -> f Bool
p ||| p' = (||) <$> p <*> p'

note :: a -> Maybe b -> Either a b
note _ (Just x) = Right x
note e Nothing = Left e