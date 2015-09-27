{-# LANGUAGE Rank2Types #-}
module Main where

import Test.Check
import Week01.List

containsUBC :: List String -> Bool
containsUBC = template False isUBC where
  isUBC x rest = rest || x == "UBC"

testSumLon :: IO ()
testSumLon = do
  checkExpect "sumLon empty"  (sumLon empty) 0
  checkExpect "sumLon one element" (sumLon (cons 60 empty)) 60
  checkExpect "sumLon two elements" (sumLon (cons 60 (cons 42 empty))) 102

sumLon :: List Int -> Int
sumLon = template 0 (+)

main :: IO ()
main = do
  testSumLon
