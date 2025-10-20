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

a :: Array Int
a = do
        x <- [2]
        pure (1 + x)


-- I'm sorry, but the syntax for collection monads is counterintuitive.
-- The explicitly stated syntactic return value of is just a collection,
-- but then you get *multiple* collections like that, *and then* they
-- are concatenated.  That is non-intuitive and non-transparent.
-- And if your return value looks like a singleton array or list, it's not (necessarily).  
-- And they said that monads weren't actually confusing.
-- This is also what Purescript wants us to use to replace list comprehensions.
-- These all do the same thing, returning [2,3,4]:
inked1 = 1..3 >>= \i -> [(i + 1)]
inked2 = 1..3 >>= \i -> (i + 1) : []
inked3 = 1..3 >>= \i -> pure (i + 1)
inked4 = do
        i <- 1..3
        [(i + 1)]
inked5 = do
        i <- 1..3
        (i + 1) : []
inked6 = do
        i <- 1..3
        pure (i + 1)

main :: Effect Unit
main = do
        log $ "pairs1: " <> (show pairs1)
        log $ "pairs2: " <> (show pairs2)
        log $ "pairs3: " <> (show pairs3)
        log $ "inked1: " <> (show inked1)
        log $ "inked2: " <> (show inked2)
        log $ "inked3: " <> (show inked3)
        log $ "inked4: " <> (show inked4)
        log $ "inked5: " <> (show inked5)
        log $ "inked6: " <> (show inked6)
        {-
        logShow (10..14)  -- the parens are just delimiters
        logShow $ 10..14  -- this way works as well
        logShow [10..14]  -- array of array
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
