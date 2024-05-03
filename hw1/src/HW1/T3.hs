module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (size) _ _ _) = size

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left a right = Branch (tsize left + tsize right + 1) left a right

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch _ left _ right) = max (tdepth left) (tdepth right) + 1

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember n (Branch _ left value right) | n < value  = tmember n left
                                      | n > value  = tmember n right
                                      | otherwise = True

tleft :: Tree a -> Tree a
tleft Leaf = Leaf
tleft (Branch _ left _ _) = left

tright :: Tree a -> Tree a
tright Leaf = Leaf
tright (Branch _ _ _ right) = right

tvalue :: Tree a -> Maybe a
tvalue Leaf = Nothing
tvalue (Branch _ _ value _) = Just value

bfactor :: Tree a -> Int
bfactor Leaf = 0
bfactor (Branch _ left _ right) = tdepth right - tdepth left

leftRotate :: Tree a -> Tree a
leftRotate Leaf = Leaf
leftRotate tree@(Branch _ left value right) = case tvalue right of
  Nothing -> tree
  Just rightValue -> mkBranch (mkBranch left value (tleft right)) rightValue (tright right)

rightRotate :: Tree a -> Tree a
rightRotate Leaf = Leaf
rightRotate tree@(Branch _ left value right) = case tvalue left of
  Nothing -> tree
  Just leftValue -> mkBranch (tleft left) leftValue (mkBranch (tright left) value right)

bigLeftRotate :: Tree a -> Tree a
bigLeftRotate Leaf = Leaf
bigLeftRotate (Branch _ left value right) = leftRotate (mkBranch left value (rightRotate right)) 

bigRightRotate :: Tree a -> Tree a
bigRightRotate Leaf = Leaf
bigRightRotate (Branch _ left value right) = rightRotate (mkBranch (leftRotate left) value right) 

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance tree@(Branch _ left node right) =
  case bfactor tree of
    2 -> case bfactor right of 
          -1 -> bigLeftRotate tree
          _ -> leftRotate tree
    -2 -> case bfactor left of 
          1 -> bigRightRotate tree
          _ -> rightRotate tree
    _ -> tree

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert n Leaf = mkBranch Leaf n Leaf
tinsert n tree@(Branch _ left value right) = let p | n < value = mkBranch (tinsert n left) value right
                                                   | n > value = mkBranch left value (tinsert n right)
                                                   | otherwise = tree
                                             in balance p                                        

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf