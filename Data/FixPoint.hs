{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
module Data.FixPoint where

data Void
data Least
data Greatest

newtype FixPhi p f = In { out :: f (FixPhi p f) }

type Fix = FixPhi Void

newtype Algebra f a = Algebra (f a -> a)

newtype PsiAlgebraPhi p f a = PsiAlgebra (f (a, FixPhi p f) -> a)

type PsiAlgebra f a = PsiAlgebraPhi Void f a

newtype CoAlgebra f a = CoAlgebra (a -> f a)

newtype CoPsiAlgebraPhi p f a = CoPsiAlgebra (a -> f (Either a (FixPhi p f)))

type CoPsiAlgebra f a = CoPsiAlgebraPhi Void f a

diag :: (a -> b) -> (a -> c) -> a -> (b,c)
f `diag` g = \x -> (f x, g x)

catamorphism :: (Functor f) => Algebra f a -> FixPhi p f -> a
catamorphism a@(Algebra f) x = f (fmap (catamorphism a) (out x))

catamorphismL :: (Functor f) => Algebra f a -> FixPhi Least f -> a
catamorphismL = catamorphism

paramorphism :: (Functor f) => PsiAlgebraPhi p f a -> FixPhi p f -> a
paramorphism (PsiAlgebra f) = fst . catamorphism (Algebra (f `diag` (In . fmap snd)))

anamorphism :: (Functor f) => CoAlgebra f a -> a -> FixPhi p f
anamorphism ca@(CoAlgebra f) x = In (fmap (anamorphism ca) (f x))

anamorphismG :: (Functor f) => CoAlgebra f a -> a -> FixPhi Greatest f
anamorphismG = anamorphism
