module Scratch where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)

import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.State.Class

import Control.Plus (empty)
import Data.Array ((..))

countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n
    then pure [ x, y ]
    else empty




main :: Effect Unit
main = do
       log "25"
