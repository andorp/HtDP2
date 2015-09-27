{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.FixPoint

data N a
  = Empty
  | Cons a
  deriving Show

instance Functor N where
  fmap _ Empty = Empty
  fmap f (Cons a) = Cons (f a)

type Natural = Fix N

isZero :: Natural -> Bool
isZero (In Empty) = True
isZero _ = False

add1 :: Natural -> Natural
add1 n = In (Cons n)

sub1 :: Natural -> Natural
sub1 (In (Cons n)) = n

algebra :: a -> (a -> a) -> Algebra N a
algebra zero sub1 = Algebra initial where
  initial Empty = zero
  initial (Cons x) = sub1 x

template :: a -> (a -> a) -> Natural -> a
template zero sub1 = catamorphism $ algebra zero sub1

add :: Natural -> Natural -> Natural
add a b = template a add1 b

natFromInteger :: Integer -> Natural
natFromInteger = anamorphism $ CoAlgebra convert where
  convert 0 = Empty
  convert n = Cons (n - 1)

natToInteger :: Natural -> Integer
natToInteger = catamorphism $ Algebra convert where
  convert Empty = 0
  convert (Cons n) = n + 1

instance Show Natural where
  show = show . natToInteger

instance Num Natural where
  (+) = add
  (-) = undefined
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = natFromInteger
