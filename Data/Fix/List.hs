{-# LANGUAGE Rank2Types #-}
module Data.Fix.List where

import Data.FixPoint

data L a b
  = Empty
  | Cons a b
  deriving (Eq, Show)

instance Functor (L a) where
  fmap _ Empty      = Empty
  fmap f (Cons a b) = Cons a (f b)

type List a = Fix (L a)

algebra :: b -> (a -> b -> b) -> Algebra (L a) b
algebra emptyCase consCase = Algebra initial where
  initial Empty      = emptyCase
  initial (Cons a b) = consCase a b


template :: b -> (a -> b -> b) -> List a -> b
template emptyCase consCase = catamorphism $ algebra emptyCase consCase

referenceAlgebra :: b -> (a -> c) -> (c -> b -> b) -> Algebra (L a) b
referenceAlgebra emptyCase helper consCase = Algebra initial where
  initial Empty      = emptyCase
  initial (Cons a b) = consCase (helper a) b

referenceTemplate :: b -> (a -> c) -> (c -> b -> b) -> List a -> b
referenceTemplate emptyCase helper consCase =
   catamorphism $ referenceAlgebra emptyCase helper consCase

lengthAlgebra :: Algebra (L a) Int
lengthAlgebra = algebra 0 (const (+1))

coAlgebra :: (b -> Maybe (a, b)) -> CoAlgebra (L a) b
coAlgebra f = CoAlgebra final where
  final = maybe Empty (uncurry Cons) . f

builder :: (b -> Maybe (a,b)) -> b -> List a
builder construct = anamorphism $ coAlgebra construct

listLength :: List a -> Int
listLength = catamorphism lengthAlgebra

isEmpty :: List a -> Bool
isEmpty (In Empty)      = True
isEmpty (In (Cons _ _)) = False

empty :: List a
empty = In Empty

cons :: a -> List a -> List a
cons a as = In (Cons a as)

fromList :: [a] -> List a
fromList = anamorphism $ coAlgebra coAlg where
  coAlg []     = Nothing
  coAlg (x:xs) = Just (x, xs)

toList :: List a -> [a]
toList = catamorphism $ algebra [] (:) where
