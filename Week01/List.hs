{-# LANGUAGE Rank2Types #-}
module Week01.List where

import Data.FixPoint

data ListShape a b
  = Empty
  | Cons a b
  deriving (Eq, Show)

instance Functor (ListShape a) where
  fmap _ Empty      = Empty
  fmap f (Cons a b) = Cons a (f b)

type List a = Fix (ListShape a)

algebra :: b -> (a -> b -> b) -> Algebra (ListShape a) b
algebra emptyCase consCase = Algebra initial where
  initial Empty      = emptyCase
  initial (Cons a b) = consCase a b

template :: b -> (a -> b -> b) -> List a -> b
template emptyCase consCase = catamorphism $ algebra emptyCase consCase

referenceAlgebra :: b -> (a -> c) -> (c -> b -> b) -> Algebra (ListShape a) b
referenceAlgebra emptyCase helper consCase = Algebra initial where
  initial Empty      = emptyCase
  initial (Cons a b) = consCase (helper a) b

referenceTemplate :: b -> (a -> c) -> (c -> b -> b) -> List a -> b
referenceTemplate emptyCase helper consCase =
   catamorphism $ referenceAlgebra emptyCase helper consCase

lengthAlgebra :: Algebra (ListShape a) Int
lengthAlgebra = algebra 0 (const (+1))

listLength :: List a -> Int
listLength = catamorphism lengthAlgebra

isEmpty :: List a -> Bool
isEmpty (In Empty)      = True
isEmpty (In (Cons _ _)) = False

empty :: List a
empty = In Empty

cons :: a -> List a -> List a
cons a as = In (Cons a as)
