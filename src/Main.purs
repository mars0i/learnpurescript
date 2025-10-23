module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Number
import Data.Maybe
-- import Data.Array
-- import Data.List
import Data.List.Lazy

ricker k r n = n * exp (r * (1.0 - n/k))

-- A lazy list iterate
ricks k r init = iterate (ricker k r) init


main :: Effect Unit
main = do
        logShow $ take 4 (ricks 1000.0 3.0 0.25)
