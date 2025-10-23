module Main where

-- If lsp complains that these don't exist, try running
-- spago build or spago run.
import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Number as N
import Data.Int as I
import Data.Maybe
import Data.Array

s :: String
s = "this " <> "that"

x1 :: Number
x1 = 16.0

n :: Int
n = -4 + -1

z1 :: Int
z1 = 27

maybeint :: Maybe Int
maybeint = I.fromNumber x1
maybenum :: Maybe Number
maybenum = Just 25.0

-- No type sig generates a MissingTypeDeclaration warning, but we can suppress that in vimrc
maybenum2 = Just 32.0

nn :: Number
nn = I.toNumber n

-- Note in Haskell list ranges have to be enclosed in [].
-- In purescript this is true neither of array nor list ranges;
-- ".." is an operator.

pairs1 = 1..3 >>= \x -> 100..101
              >>= \y -> [x,y]  -- generates array of alternating w's and z's

pairs2 = do 
        w <- 1..3
        z <- 100..101
        [w,z]  -- generates array of alternating w's and z's

pairs3 = do 
        w' <- 1..3
        z' <- 100..101
        pure [w',z']  -- generates array of array-pairs

f :: Int -> Int
f n = if n > 0
        then 1 + f (n - 1)
        else 0

g :: Int -> Int -> Int
g a n = if n > 0
        then g (a + 1) (n - 1)
        else a



main :: Effect Unit
main = do
        logShow pairs1
        logShow pairs2
        logShow pairs3
        logShow (10..14)  -- the parens are just delimiters
        logShow $ 10..14  -- this way works as well
        logShow [10..14]  -- array of array
        {-
        log ""
        logShow maybeint
        logShow maybenum
        logShow maybenum2
        let nine = 9
        logShow nine
        log s
        log (show z1)
        logShow z1
        log $ show $ I.pow 5 4
        log $ show $ N.pow x1 nn
        log $ show $ N.pow x1 3.0
        log $ show $ N.pow x1 (- nn)
        log (show (N.pow 2.0 0.5))
        log (show n)
        log "🍝"
        -}


main :: Effect Unit
main = do
        logShow pairs1
        logShow pairs2
        logShow pairs3
        logShow (10..14)  -- the parens are just delimiters
        logShow $ 10..14  -- this way works as well
        logShow [10..14]  -- array of array
        {-
        log ""
        logShow maybeint
        logShow maybenum
        logShow maybenum2
        let nine = 9
        logShow nine
        log s
        log (show z1)
        logShow z1
        log $ show $ I.pow 5 4
        log $ show $ N.pow x1 nn
        log $ show $ N.pow x1 3.0
        log $ show $ N.pow x1 (- nn)
        log (show (N.pow 2.0 0.5))
        log (show n)
        log "🍝"
        -}
