-- Example using both versions of guard:
module TwoGuards where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Array ((..)) -- i.e. import the range operator: ..
import Data.EuclideanRing (mod)
import Control.Alternative (guard) as A
import Data.Monoid (guard) as M

-- Example using both versions of guard:
divBy3and5 :: Int -> Array Int
divBy3and5 n = do
  i <- 1..n
  A.guard (i `mod` 3 == 0)
  M.guard (i `mod` 5 == 0) (pure i)
  


main :: Effect Unit
main = do
       logShow $ divBy3and5 100
