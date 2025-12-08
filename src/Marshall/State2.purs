module Marshall.State2 where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Effect.Console (log, logShow)

import Control.Monad.State
import Control.Monad.State.Class
import Data.Tuple
import Data.List
-- import Data.Maybe
-- import Data.Foldable (traverse_, for_)

-- See also
-- https://brandon.si/code/the-state-monad-a-tutorial-for-the-confused:
-- Lipovaca pp. 316ff

-- From https://www.cs.bu.edu/fac/snyder/cs320/Lectures/Lecture12--%20State%20Monad.pdf

type Stack = List Int

-- In Haskell, you can put a back in the "outer state", but in Purescript,
-- that means that you have to add `_ <-` in `do`.  But this means other
-- functions, such as `mult`, have to be changed.
push :: Int -> State Stack Unit
push a = state $ \xs -> (Tuple unit (Cons a xs))

pop :: State Stack Int
pop = unsafePartial $ state $ \(Cons x xs) -> (Tuple x xs)
-- pop = state $ \Nil -> (Tuple 0 Nil)

mult :: State Stack Unit
mult = pop >>= \x -> pop
           >>= \y -> push (x * y)

domult :: State Stack Unit
domult = do x <- pop
            y <- pop
            push (x * y)

-- usage: runState prog1 <somelist>
prog1 :: State Stack Int
prog1 = push 2 >>= (\_ -> (push 5)
               >>= (\_ -> (push 8)
               >>= (\_ -> pop
               >>= (\x -> pop
               >>= (\y -> (push (x - y))
               >>= (\_ -> mult
               >>= (\_ -> pop)))))))

-- usage: runState prog1 <somelist>
-- Same as prog1 because lambdas extend as far to the right as possible.
-- https://stackoverflow.com/a/79837542/1455243
prog2 :: State Stack Int
prog2 = push 2 >>= \_ -> push 5
               >>= \_ -> push 8
               >>= \_ -> pop
               >>= \x -> pop
               >>= \y -> push (x - y)
               >>= \_ -> mult
               >>= \_ -> pop

-- usage: runState prog1 <somelist>
-- Same but using the do version of mult
prog3 :: State Stack Int
prog3 = push 2 >>= \_ -> push 5
               >>= \_ -> push 8
               >>= \_ -> pop
               >>= \x -> pop
               >>= \y -> push (x - y)
               >>= \_ -> domult
               >>= \_ -> pop

-- usage: runState prog1 <somelist>
-- Same as prog3 but using *>
prog4 :: State Stack Int
prog4 = push 2 *> push 5
               *> push 8
               *> pop
               >>= \x -> pop
               >>= \y -> push (x - y)
               *> domult
               *> pop

prog5 :: State Stack Int
prog5 = do push 2 
           push 5
           push 8
           x <- pop
           y <- pop
           push (x - y)
           mult
           pop

-------------------------------

main :: Effect Unit
main = void $ unsafePartial do
   log "hey!"
