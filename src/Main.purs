module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Number
import Data.Maybe
import Data.Array as A
import Data.List as L
import Data.List.Lazy as LL
import Effect.Random

ricker :: Number → Number → Number → Number
ricker k r n = n * exp (r * (1.0 - n/k))

-- A lazy list iterate
ricks k r init = LL.iterate (ricker k r) init

seedless :: Effect Number -> Effect Number
seedless _ = random

randnos = LL.iterate seedless (pure 0.0)

foo :: (Int * Number) -> Number -> String
foo _ _ = ?foo

{-
main :: Effect Number
main = do
        logShow $ LL.take 4 (ricks 1000.0 3.0 0.25)
        logShow $ map (LL.take 10) randnos
        -}

main :: Effect Unit
main = do
        randno <- random
        logShow randno
        pure unit
