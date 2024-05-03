module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ start Leaf                        = start 
tfoldr f start (Branch _ left value right) = tfoldr f (value `f` (tfoldr f start right)) left
