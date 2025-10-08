module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

x :: String
x = "this " <> "that"

main :: Effect Unit
main = do
        log "Yow!"
        log x
        log "🍝"


