module Main where

-- If lsp complains that these don't exist, try running
-- spago build or spago run.
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
n = -4 + -1

z :: Int
z = 27

-- maybeint :: Maybe Int
-- maybeint = fromNumber x

nn :: Number
nn = I.toNumber n


main :: Effect Unit
main = do
        log "Yow!"
        log s
        log (show z)
        log $ show $ I.pow 5 4
        log $ show $ N.pow x nn
        log $ show $ N.pow x 3.0
        log $ show $ N.pow x (- nn)
        log (show (N.pow 2.0 0.5))
        log (show n)
        log "🍝"
