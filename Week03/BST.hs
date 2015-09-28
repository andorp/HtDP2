module Week03.BST where

import Data.FixPoint

data Bst k v b
  = Leaf
  | Node k v b b
  deriving (Eq, Show)

-- INVARIANTS:
--  * For a given node key is greater than all keys in its left child
--  * For a given node key is less then all keys in its right child
--  * The same key never appears twice in the tree

instance Functor (Bst k v) where
  fmap _ Leaf = Leaf
  fmap f (Node k v l r) = Node k v (f l) (f r)

type BST k v = Fix (Bst k v)

leaf :: BST k v
leaf = In Leaf

node :: k -> v -> BST k v -> BST k v -> BST k v
node k v l r = In (Node k v l r)

