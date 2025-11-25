module Marshall.State where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Effect.Console (log, logShow)

import Control.Monad.State
import Data.Tuple
import Data.Identity
import Data.Array
import Data.Maybe

-- From Hutton sect 12.3

data HTree a = HLeaf a | HNode (HTree a) (HTree a)

{-
fresh :: State Int
fresh :: State (\n -> Tuple n (n+1))
-}


-- Based on Thomson _Haskell: The Craft of Functional Programming", 3rd ed. 2023,
-- section 18.6, pp. 505ff

deidentity :: forall a. Identity a -> a
deidentity (Identity x) = x

data Tree a = Leaf | Node a (Tree a) (Tree a)

mynumtree :: Tree Int
mynumtree = Node 25
                 (Node 3
                       Leaf
                       (Node 1 Leaf Leaf))
                 (Node 7
                       (Node 5 Leaf Leaf)
                       Leaf)

zappakids :: Tree String
zappakids = Node "Moon"
                 (Node "Ahmet" Leaf Leaf)
                 (Node "Dweezil"
                        (Node "Ahmet" Leaf Leaf)
                        (Node "Moon" Leaf Leaf))

-------------------------------

sumTree :: Tree Int -> Identity Int
sumTree Leaf = pure 0
sumTree (Node n t1 t2) = do
        num <- pure n
        s1 <- sumTree t1
        s2 <- sumTree t2
        pure (num + s1 + s2)

undoneSumTree :: Tree Int -> Identity Int
undoneSumTree Leaf = pure 0
undoneSumTree (Node n t1 t2) =
        pure n >>=
        \num -> undoneSumTree t1 >>=
        \s1 -> undoneSumTree t2 >>=
        \s2 -> pure (num + s1 + s2)

-------------------------------

-- Remember that 'type' defines an alias and not a new type.
type Table a = Array a  -- [a] can't be used as a type name in Purescript, apparently

-- Note trick: We want a function from a to a function from Table to Tuple.
-- But we use currying to references the second "argument" in the def.
nNode :: forall a. Eq a => a -> Table a -> (Tuple (Table a) Int)
nNode x table = case (elemIndex x table) of   -- avoids redundancy of Thompson's
                     Just k -> Tuple table k  -- if item is in table, use its index
                     Nothing -> Tuple (table <> [x]) (length table) -- else add it



numberNode :: forall a. Eq a => a -> State a Int
numberNode x = state (nNode x)


numberTree :: forall a. Eq a => Tree a -> State a (Tree Int)
numberTree Leaf = pure Leaf
numberTree (Node x t1 t2) =
        do num <- numberNode x
           nt1 <- numberTree t1
           nt2 <- numberTree t2
           pure (Node num nt1 nt2)



-------------------------------

main :: Effect Unit
main = void $ unsafePartial do
  log "Values should be 41:"
  logShow $ deidentity $ sumTree mynumtree
  logShow $ deidentity $ undoneSumTree mynumtree


