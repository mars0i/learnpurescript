module Marshall.State where

import Control.Monad.State

type Stack = [Integer]

push :: Integer -> State Stack Integer
push a = state $ \xs -> (a, a:xs)

pop :: State Stack Integer
pop = state $ \(x:xs) -> (x, xs)

mult :: State Stack Integer
mult = do x <- pop
          y <- pop
          push (x * y)

-- usage: runState prog <somelist>
prog :: State Stack Integer
prog = push 2 >>= (\_ -> (push 5)
              >>= (\_ -> (push 8)
              >>= (\_ -> pop
              >>= (\x -> pop
              >>= (\y -> (push (x - y))
              >>= (\z -> mult
              >>= (\_ -> pop)))))))


-- usage: runState prog <somelist>
prog2 :: State Stack Integer
prog2 = do push 2 
           push 5
           push 8
           x <- pop
           y <- pop
           z <- push (x - y)
           mult
           push z
           mult
           pop
