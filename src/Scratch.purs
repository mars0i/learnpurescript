module Scratch where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Array
import Data.EuclideanRing
import Control.Alternative (guard) as A
import Data.Monoid (guard) as M
import Data.Maybe

bob :: Maybe Number -> Maybe Number -> Maybe Number
bob (Just x) (Just y) = Just (x / y)
bob _ _ = Nothing


main :: Effect Unit
main = do
       log "25"
