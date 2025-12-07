module MWE where

import Prelude
import Partial.Unsafe (unsafePartial)
import Control.Monad.State
import Data.List
import Data.Tuple

push :: Int -> State (List Int) Int
push a = state $ \xs -> (Tuple a (Cons a xs))

pop :: State (List Int) Int
pop = unsafePartial $ state $ \(Cons x xs) -> (Tuple x xs)

prog1 :: State (List Int) Int
prog1 = push 2 *>
        pop >>=
        \x -> push (- x) *>
        pop

-- It looks like the explicit '_ <-' is required unless the returned
-- value has been declared an instance of `Control.Bind.Discard, 
-- as for example Unit has been.
prog2 :: State (List Int) Int
prog2 = do _ <- push 2
           x <- pop
           _ <- push (- x)
           pop
