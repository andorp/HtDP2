module Week03.MutualReference where

import Data.FixPoint
import Data.Fix.List (List, L(..))

data Element = Element String Integer (List Element)

template :: (String -> Integer -> e -> a) -> e -> (a -> e -> e) -> Element -> a
template f empty cons = fnForE where

  fnForLoe = catamorphism $ Algebra initial where
    initial Empty       = empty
    initial (Cons e es) = cons (fnForE e) es

  fnForE (Element name data_ es) = f name data_ (fnForLoe es)
