module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Number as N
import Data.Int as I

s :: String
s = "this " <> "that"

x :: Number
x = 16.0

n :: Int
n = -4

-- maybeint :: Maybe Int
-- maybeint = fromNumber x

nn :: Number
nn = I.toNumber n


main :: Effect Unit
main = do
        log "Yow!"
        log s
        log $ show $ I.pow 5 3
        log $ show $ N.pow x nn
        log $ show $ N.pow x 2.0
        log $ show $ N.pow x (- nn)
        log (show n)
        log "🍝"


