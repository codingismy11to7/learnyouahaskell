module BST
  ( Tree(..)
  , findNode
  , sampleTree
  , emptyTree
  , singleton
  , insert
  ) where

data Tree a
  = Leaf
  | Node { value        :: a
         , smallerNodes :: Tree a
         , largerNodes  :: Tree a }
  deriving (Eq, Show, Read)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node value smaller larger) = Node (f value) (fmap f smaller) (fmap f larger)

emptyTree :: Tree v
emptyTree = Leaf

findNode :: (Ord v) => v -> Tree v -> Maybe (Tree v)
findNode _ Leaf = Nothing
findNode v n
  | value n == v = Just n
  | value n < v = findNode v $ largerNodes n
  | otherwise = findNode v $ smallerNodes n

{-
sampleTree :: Tree Integer
sampleTree = Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 7 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf))
-}
sampleTree :: Tree Integer
sampleTree = foldr insert Leaf [1, 4, 6, 9, 3, 7, 5]

singleton :: x -> Tree x
singleton x = Node x Leaf Leaf

insert :: (Ord v) => v -> Tree v -> Tree v
insert x Leaf = singleton x
insert x n
  | value n == x = n
  | value n < x = Node (value n) (smallerNodes n) (insert x (largerNodes n))
  | otherwise = Node (value n) (insert x (smallerNodes n)) (largerNodes n)
