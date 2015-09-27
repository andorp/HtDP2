module Main where

import Data.FixPoint
import Data.Fix.List
import Data.Image hiding (Empty)

blank = Square 0 Solid White

-- Sort images in increasing order of size and then lay them out left to right
arrangeImages :: List Image -> Image
arrangeImages = layoutImage . sortImage

-- place images beside each other in order of list
layoutImage :: List Image -> Image
layoutImage = template blank beside where
  beside im (Beside images) = Beside (im:images)

-- sort images in order of increasing order of size (area)
sortImage :: List Image -> List Image
sortImage = catamorphism $ Algebra insert
  where
    insert = anamorphism $ CoAlgebra ins

    ins Empty = Empty
    ins (Cons a (In Empty)) = Cons a Empty
    ins (Cons a (In (Cons b x)))
      | larger b a = Cons a (Cons b x)
      | otherwise  = Cons b (Cons a x)

larger :: Image -> Image -> Bool
larger i1 i2 = (width i1 * height i1) > (width i2 * height i1)

{-
-- Ordered List
type LO a = L a

bsort :: (Ord a) => List a -> List a
bsort = anamorphism $ CoAlgebra bubble

bubble :: (Ord a) => List a -> L a (List a)
bubble = catamorphism $ Algebra bub

bub :: Ord a => L a (L a (List a)) -> L a (List a)
bub Empty = Empty
bub (Cons a Empty) = Cons a (In Empty)
bub (Cons a (Cons b x))
  | a <= b    = Cons a (In (Cons b x))
  | otherwise = Cons b (In (Cons a x))
-}

main = do
  undefined
