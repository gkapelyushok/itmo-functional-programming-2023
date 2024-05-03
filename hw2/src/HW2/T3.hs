module HW2.T3
  ( epart
  , mcat
  ) where

import Data.Foldable
import Data.Monoid()

mcat :: Monoid a => [Maybe a] -> a
mcat = fold . fold

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap f
  where
    f (Left a)  = (a, mempty)
    f (Right b) = (mempty, b) 
