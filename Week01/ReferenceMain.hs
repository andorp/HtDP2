{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Week01.List
import Test.Check
import Data.Image

-- Tituition fee in USD
newtype Tituition = Tituition { tituition :: Int }
  deriving (Eq, Show, Num)

data School = School { schoolName :: String, schoolTituition :: Tituition }

yScale = 800

fontSize  = 32
fontColor = Black

barColor = Green
barWidth = 100

-- Produce the bar for a single school in the bar chart
makeBar :: School -> Image
makeBar s = OverlayAlign Center Bottom
              [ Rotate 90 $ (Text $ schoolName s) fontSize fontColor
              , Rectangle barWidth (tituition $ schoolTituition s * yScale) Outline Black
              , Rectangle barWidth (tituition $ schoolTituition s * yScale) Solid barColor
              ]

chart :: List School -> Image
chart = referenceTemplate (Square 0 Solid White) makeBar (besideAlign Bottom)

main :: IO ()
main = do
  undefined
