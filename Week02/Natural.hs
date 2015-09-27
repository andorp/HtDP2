module Main where

import Data.FixPoint

data NatS a
  = Zero
  | Suc a
  deriving (Eq, Show)

instance Functor NatS where
  fmap _ Zero    = Zero
  fmap f (Suc n) = Suc (f n)

type Nat = Fix NatS

add1 n = In (Suc n)
sub1 (In (Suc n)) = n

algebra :: a -> (a -> a) -> Algebra NatS a
algebra zero suc = Algebra initial where
  initial Zero    = zero
  initial (Suc a) = suc a

psiAlgebra :: a -> (Fix NatS -> b) -> (b -> a -> a) -> PsiAlgebra NatS a
psiAlgebra zero value suc = PsiAlgebra initial where
  initial Zero         = zero
  initial (Suc (a, x)) = suc (value x) a

template :: a -> (Nat -> b) -> (b -> a -> a) -> Nat -> a
template zero value suc = paramorphism $ psiAlgebra zero value suc
