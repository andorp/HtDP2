module Week03.BST where

import Data.FixPoint
import Data.Image (Image)
import Data.Image as Image

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

algebra :: a -> (k -> v -> a -> a -> a) -> Algebra (Bst k v) a
algebra leaf node = Algebra initial where
  initial Leaf           = leaf
  initial (Node k v l r) = node k v l r

template :: a -> (k -> v -> a -> a -> a) -> BST k v -> a
template leaf node = catamorphism $ algebra leaf node

lookup :: (Eq k, Ord k) => k -> BST k v -> Maybe v
lookup key = template leaf node where
  leaf = Nothing
  node k v l r
    | k == key = Just v
    | k <  key = l
    | k >  key = r
    | otherwise = error "BST.lookup impossible case"

-- * Render of BST

textSize = 14
textColor = Image.Black
keyValSep = ":"
vspace = Image.Rectangle 1 10 Solid White
hspace = Image.Rectangle 10 1 Solid White

render :: (Show k, Show v) => BST k v -> Image
render = template leaf node where
  leaf = Image.Empty
  node k v l r
    = Image.Above [ Image.Text (concat [show k, keyValSep, show v])
                               textSize
                               textColor
                  , vspace
                  , Image.Beside [ l, hspace, r ]
                  ]
