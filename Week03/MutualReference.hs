module Week03.MutualReference where

import           Control.Monad (mplus)

import           Data.FixPoint
import           Data.Fix.List (List, L(..))
import qualified Data.Fix.List as List
import           Test.Check

data Element = Element String Integer (List Element)

template :: (String -> Integer -> e -> a) -> e -> (a -> e -> e) -> Element -> a
template f empty cons = fnForE where

  fnForLoe = catamorphism $ Algebra initial where
    initial Empty       = empty
    initial (Cons e es) = cons (fnForE e) es

  fnForE (Element name data_ es) = f name data_ (fnForLoe es)

-- Design a function that consumes Element and produces the sum of all the file data in the tree

f1 = Element "F1" 1 List.empty
f2 = Element "F2" 2 List.empty
f3 = Element "F3" 3 List.empty
d4 = Element "D4" 0 (List.fromList [f1, f2])
d5 = Element "D5" 0 (List.fromList [f3])
d6 = Element "D6" 0 (List.fromList [d4, d5])

sumElement :: Element -> Integer
sumElement = template element 0 (+)
  where
    element _name 0 value = value
    element _name n _     = n

testSum = do
  checkExpect "Sum F1" 1 (sumElement f1)
  checkExpect "Sum D6" 6 (sumElement d6)

-- Design a function that produces the list of the names in the element

listOfNames :: Element -> [String]
listOfNames = template element [] (++) where
  element name _ value = name:value

testListOfNames = do
  print $ listOfNames d6

-- Design a function that produces that consumes a string and an Element and looks for a data
-- element with the given name. If it finds that element it produces (Just data) otherwise Nothing

findElement :: String -> Element -> Maybe Integer
findElement name = template element Nothing mplus
  where
    element name' data_ v1 = v1 `mplus` v2
      where
        v2 = if name' == name then Just data_ else Nothing

testFindElement = do
  checkExpect "findElement F1" (Just 1) (findElement "F1" d6)
  checkExpect "findElement D6" (Just 0) (findElement "D6" d6)
  checkExpect "findElement D4" (Just 0) (findElement "D4" d6)
  checkExpect "findElement G1" Nothing (findElement "G1" d6)
