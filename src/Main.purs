module Main where

-- If lsp complains that these don't exist, try running
-- spago build or spago run.
import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Number as N
import Data.Int as I
import Data.Maybe
-- import Data.Array

s :: String
s = "this " <> "that"

x :: Number
x = 16.0

n :: Int
n = -4 + -1

z :: Int
z = 27

maybeint :: Maybe Int
maybeint = I.fromNumber x
maybenum :: Maybe Number
maybenum = Just 25.0

maybenum2 = Just 32.0

nn :: Number
nn = I.toNumber n


main :: Effect Unit
main = do
        logShow maybeint
        logShow maybenum
        logShow maybenum2
        let nine = 9
        logShow nine
        log s
        log (show z)
        logShow z
        log $ show $ I.pow 5 4
        log $ show $ N.pow x nn
        log $ show $ N.pow x 3.0
        log $ show $ N.pow x (- nn)
        log (show (N.pow 2.0 0.5))
        log (show n)
        log "🍝"
