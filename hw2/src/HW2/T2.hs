module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr helper ([] :| [])
  where helper cur (h :| t) =  if cur == sep
                               then [] :| (h : t)
                               else (cur : h) :| t

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep (h :| t) = foldl (\x xs -> x ++ (sep : xs)) h t
