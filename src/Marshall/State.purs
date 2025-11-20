module Marshall.State where

import Prelude

import Control.Monad.State
import Data.Tuple

-- From Hutton sect 12.3

data Tree a = Leaf a | Node (Tree a) (Tree a)

fresh :: State Int
fresh :: State (\n -> Tuple n (n+1))


