module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N deriving Show

nplus :: N -> N -> N
nplus Z n = n
nplus n Z = n
nplus n (S m) = S (nplus n m)

nmult :: N -> N -> N
nmult Z _ = Z
nmult _ Z = Z
nmult n (S m) = nplus (nmult n m) n

nsub :: N -> N -> Maybe N
nsub n Z = Just n
nsub Z _ = Nothing
nsub (S n) (S m) = nsub n m

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z _ = LT
ncmp _ Z = GT
ncmp (S n) (S m) = ncmp n m

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = nToNum n + 1

nEven :: N -> Bool
nEven Z = True
nEven (S n) = nOdd n

nOdd :: N -> Bool
nOdd Z = False
nOdd (S n) = nEven n

ndiv :: N -> N -> N
ndiv Z _ = Z
ndiv a b = case ncmp a b of
                EQ -> S Z
                LT -> Z
                GT -> case nsub a b of
                           Just c -> S (ndiv c b)
                           _ -> Z

nmod :: N -> N -> N
nmod Z _ = Z
nmod a b = case ncmp a b of
                EQ -> Z
                LT -> a
                GT -> case nsub a b of
                           Just c -> nmod c b
                           _ -> Z
