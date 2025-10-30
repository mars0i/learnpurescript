module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Data.Foldable (for_, traverse_)
import Data.Number (exp)
import Data.Maybe
import Data.Array
import Data.List as L
import Data.List.Lazy as LL
import Effect.Random
-- import Jack  -- not needed?
-- import Jack.Seed as JS
-- import Jack.Rand as JR
import Control.Alternative (guard)
import Data.Monoid (guard) as Monoid -- This guard has a different syntax and semantics

import Data.Int.Bits as Bits
import Data.UInt as UInt

ricker :: Number → Number → Number → Number
ricker k r n = n * exp (r * (1.0 - n/k))

-- A lazy list iterate
ricks k r init = LL.iterate (ricker k r) init

seedless :: Effect Number -> Effect Number
seedless _ = random

n1 :: Effect Number
n1 = random

n2 :: Effect Number
n2 = n1 >>= \n -> pure (1000.0 + n)

ns :: Array (Effect Number)
ns = [n1, n2]

-- based on https://book.purescript.org/chapter5.html#do-notation
pairs :: Array (Array Int)
pairs = do
  i <- 1 .. 24
  j <- i .. 24
  guard $ i * j == 24
  pure [i, j]

pairs2 = 1..24 >>= \i -> i..24 >>= 
                   \j -> guard ((i * j) == 24) *> 
                   pure [i, j]

-- Version using Monoid's guard:
pairs3 :: Array (Array Int)
pairs3 = do
  i <- 1 .. 24
  j <- i .. 24
  Monoid.guard (i * j == 24) pure [i, j]

ns2 :: Array (Effect Number)
ns2 = do
        en1 <- ns
        pure en1

randeffects :: LL.List (Effect Number)
randeffects = LL.iterate seedless (pure 0.0)

randeffects3 :: LL.List (Effect Number)
randeffects3 = LL.take 3 $ LL.iterate seedless (pure 0.0)



randra :: Array (Effect Number)
randra = [random, random, random, random]

-- ranums = randra >>= \en -> (en >>= (\n -> pure (show n)))
-- ranums = randra >>= \en -> (en >>= (\n -> pure (show n)))


{-
main :: Effect Number
main = do
        logShow $ LL.take 4 (ricks 1000.0 3.0 0.25)
        logShow $ map (LL.take 10) randnos
        -}

maybeEffNum :: Maybe (Effect Number) -> Effect Number
maybeEffNum (Just en) = en
maybeEffNum Nothing = random *> pure (-1.0)  -- kluge: if result is negative, we know it's been Nothing'ed
-- *> is Purescript's version of Haskell's >>


main :: Effect Unit
main = do
        log "This shows how to do stuff inside a container:"
        z <- pure (_+10000.0) <*> random
        logShow z

        log ""

        log "guard illustrations:"
        logShow $ pairs
        logShow $ pairs2
        logShow $ pairs3

        log ""

        randno <- random
        randno2 <- random
        logShow randno
        logShow randno2
        logShow [randno, randno2]

        log "\nExtracting from a lazy list:\n"
        let (men :: Maybe (Effect Number)) = LL.head randeffects3 -- note head returns a Maybe
        headnum <- maybeEffNum men
        log ("head num: " <> show headnum)
        sndnum <- maybeEffNum $ LL.index randeffects3 1
        log ("second num: " <> show sndnum)
        thdnum <- maybeEffNum $ LL.index randeffects3 2
        log ("third num: " <> show thdnum)
        lastnum <- maybeEffNum $ LL.last randeffects3
        log ("last num: " <> show lastnum)
        lanoffend <- maybeEffNum $ LL.index randeffects3 3
        log $ "this is a failure: " <> (show lanoffend)

        -- OK, so I can pull numbers out of the lazy list.  How do I iterate through it?

        log "\nI can do it with arrays as well:\n"
        ran <- maybeEffNum $ head randra
        log $ "first from array: " <> (show ran)
        ran4 <- maybeEffNum $ index randra 3
        log $ "fourth from array: " <> (show ran4)
        ranoffend <- maybeEffNum $ index randra 4

        log $ "this is a failure: " <> (show ranoffend)

        log "\nAll random numbers in randra:"
        for_ randra \en -> do
           n <- en
           logShow n

        log "\nAll numbers in randeffects3:"
        for_ randeffects3 \en -> do
           n <- en
           logShow n

        log "\nThe first 10 numbers in randeffects:"
        for_ (LL.take 10 $ randeffects) \en -> do
           n <- en
           logShow n

        log ""
        logShow $ Bits.or  5 2  -- 101 | 10 = 7
        logShow $ Bits.xor 7 2  -- 111 xor 10 = 5
        logShow $ Bits.shl 3 2  -- 3 = 11 to 1100 = 12
        logShow $ Bits.shr 16 2 -- 10000 to 100 = 4
        logShow $ Bits.and 31 2 -- mask all but the 2 bit

        log ""
        logShow $ UInt.or  (UInt.fromInt 5) (UInt.fromInt 2)  -- 101 | 10 = 7
        logShow $ UInt.xor (UInt.fromInt 7) (UInt.fromInt 2)  -- 111 xor 10 = 5
        logShow $ UInt.shl (UInt.fromInt 3) (UInt.fromInt 2)  -- 3 = 11 to 1100 = 12
        logShow $ UInt.shr (UInt.fromInt 16) (UInt.fromInt 2) -- 10000 to 100 = 4
        logShow $ UInt.and (UInt.fromInt 31) (UInt.fromInt 2) -- mask all but the 2 bit

        pure unit
        
{-

        let ns = foldl (\acc en -> do 
                       n <- en
                       logShow n)
                 []
                 (randra :: Array (Effect Number))

        let ns = foldl (\acc -> \en -> do 
                       n <- en
                       n : acc) 
                 [] randra 
        logShow ns
        -}

{- Doesn't work, though do version about does:
-- I thought I could use >>> which I think is like Haskell's >>, but doesn't work.
main :: Effect Unit
main = random >>= \randno -> logShow randno
              >>= \_ -> random
              >>= \randno2 -> logShow randno2
              >>= logShow [randno, randno2]
              >>= \_ -> pure unit
              -}
