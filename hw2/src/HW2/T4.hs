module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last x) y = x :+ y
  (<>) (h :+ t) y = h :+ (t <> y) 

data Inclusive a b = This a | That b | Both a b
  deriving Show

-- You may necessary constraints there
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This i   <> This j   = This (i <> j)
  That i   <> That j   = That (i <> j)
  This i   <> That j   = Both i j
  That i   <> This j   = Both j i
  This i   <> Both j k = Both (i <> j) k
  Both i j <> This k   = Both (i <> k) j
  That i   <> Both j k = Both j (i <> k)
  Both i j <> That k   = Both i (j <> k)
  Both i j <> Both k l = Both (i <> k) (j <> l)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  (DS "") <> (DS i)  = DS i
  (DS i)  <> (DS "") = DS i
  (DS i)  <> (DS j)  = DS (i ++ ('.' : j))

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (F i) <> (F j) = F (i . j)

instance Monoid (Fun a) where
  mempty = F id
