-- Example using both versions of guard:
module TwoGuards where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Array ((..)) -- i.e. import the range operator: ..
import Data.Maybe
import Data.EuclideanRing (mod)
import Control.Alternative (guard) as A
import Data.Monoid (guard) as M

-- Example using both versions of guard:
divArrBy3and5 :: Int -> Array Int
divArrBy3and5 n = do
  i <- 1..n
  A.guard (i `mod` 3 == 0)
  M.guard (i `mod` 5 == 0) (pure i)

{- M.guard doesn't seem to work with Maybe. I thought it would. (A.guard works.)
   I think Maybe isn't an instance of Monoid.
divMaybeBy3and5 :: Maybe Int -> Maybe Int
divMaybeBy3and5 n = do
  i <- n
  A.guard (i `mod` 3 == 0)
  M.guard (i `mod` 5 == 0) (pure i)
  -}  
  
{-
Failed experiment:
dividem :: Array Int -> Array Int -> Array Int
dividem xs ys =
        [ (\x y -> M.guard (y /= 0) (x / y)) ] <*> xs <*> ys
-}


main :: Effect Unit
main = do
       logShow $ divArrBy3and5 100
