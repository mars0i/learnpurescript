module Marshall.State where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Effect.Console (log, logShow)

import Control.Monad.State
import Control.Monad.State.Class
import Data.Tuple
import Data.Identity
import Data.Array
import Data.List as L
import Data.Maybe
import Data.Foldable (traverse_, for_)

-- From Hutton sect 12.3

data HTree a = HLeaf a | HNode (HTree a) (HTree a)

{-
fresh :: State Int
fresh :: State (\n -> Tuple n (n+1))
-}


-- Based on Thompson _Haskell: The Craft of Functional Programming", 3rd ed. 2023,
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



{-
numberNode :: forall a. Eq a => a -> State a Int
numberNode x = state (nNode x)


numberTree :: forall a. Eq a => Tree a -> State a (Tree Int)
numberTree Leaf = pure Leaf
numberTree (Node x t1 t2) =
        do num <- numberNode x
           nt1 <- numberTree t1
           nt2 <- numberTree t2
           pure (Node num nt1 nt2)
-}

-- From Purescript by Example chapter 11, section "The State Monad"
-- https://book.purescript.org/chapter11.html#the-state-monad
sumArray :: Array Int -> State Int Unit
sumArray = traverse_ \n -> modify \sum -> sum + n


                         
{- My version of the following, from
https://brandon.si/code/the-state-monad-a-tutorial-for-the-confused:
(>>=) :: State s a -> (a -> State s b) -> State s b
m >>= k = State $ \s -> let (a, s') = runState m s
                         in runState (k a) s'
Note the first arg to the second runState is a function application, not a tuple. -}
blah :: forall s a b. State s a -> (a -> State s b) -> State s b
blah m k = state (\s -> let (Tuple a s') = runState m s
                         in runState (k a) s')


-- Based on https://brandon.si/code/the-state-monad-a-tutorial-for-the-confused:
fromStoAndS :: Int -> (Tuple String Int)
fromStoAndS c | (c `mod` 5) == 0 = Tuple "five-zero" (c + 1)
              | otherwise = Tuple "not-zero" (c + 1)

stateIntString :: State Int String
stateIntString = state fromStoAndS 

-------------------------------
-- From Lipovaca pp. 316ff

type Stack = L.List Int

push :: Int -> State Stack Unit
push a = state $ \xs -> (Tuple unit (L.Cons a xs))

pop :: State Stack Int
pop = unsafePartial $ state $ \(L.Cons x xs) -> (Tuple x xs)
-- pop = state $ \L.Nil -> (Tuple 0 L.Nil)

{- btw:
-- This works:
foo :: L.List Int -> Tuple Int (L.List Int)
foo L.Nil = Tuple 0 L.Nil
foo (L.Cons x xs) = Tuple x xs

-- I don't think there's a way to do this:
bar :: Array Int -> Tuple Int (Array Int)
bar [] = Tuple 0 []
bar (x:xs) = Tuple x xs
-}


-------------------------------

main :: Effect Unit
main = void $ unsafePartial do
  -- https://brandon.si/code/the-state-monad-a-tutorial-for-the-confused:
  logShow $ runState stateIntString 0
  logShow $ runState stateIntString 1
  logShow $ runState stateIntString 2
  logShow $ runState stateIntString 3
  logShow $ runState stateIntString 4
  logShow $ runState stateIntString 5
  logShow $ runState stateIntString 6
  log "\n"
  log "logShow $ runState (stateIntString *> stateIntString *> stateIntString) 3 :"
  logShow $ runState (stateIntString *> stateIntString *> stateIntString) 3
  -- This time with do:
  logShow $ runState (do
                     _ <- stateIntString -- dunno why I have to add "_ <-"
                     _ <- stateIntString
                     stateIntString) 3
  log "Q1: Why does each call to of stateIntStr only inc state once?"
  log "The def of `>==` seems to do it twice."
  log "Q2: How is the state passed along given use of `*>`?"

  -- Thompson _Haskell: The Craft of Functional Programming", pp. 505ff:
  log "\nValues should be 41:"
  logShow $ deidentity $ sumTree mynumtree
  logShow $ deidentity $ undoneSumTree mynumtree
  log "\n"
  -- based on https://book.purescript.org/chapter11.html#the-state-monad
  logShow $ runState (do sumArray [1, 2, 3]
                         sumArray [4, 5]
                         sumArray [6]) 0
  -- Same thing without do:
  logShow $ runState (sumArray [1,2,3] *> sumArray [4,5] *> sumArray [6]) 0

  log "\n"
  -- doesn't do what I'd hoped:
  logShow $ (for_ (0 .. 10) \_ -> do
     runState stateIntString) 0



