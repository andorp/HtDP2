module Data.Image where

data Fill = Solid
          | Outline
  deriving (Eq, Show)

type Size = Int

data Position = Center
              | Left
  deriving (Eq, Show)

data Color = White
           | Black
           | Green
  deriving (Eq, Show)

data Relative = Bottom
  deriving (Eq, Show)

data Image = Empty
           | Square Size Fill Color
           | Rectangle Size Size Fill Color
           | Text String Size Color

           | Rotate Int Image
           | Beside [Image]
           | BesideAlign Relative [Image]
           | OverlayAlign Position Relative [Image]

  deriving (Eq, Show)

beside im1 im2 = Beside [im1, im2]

besideAlign pos im1 im2 = BesideAlign pos [im1, im2]

-- The accumulated area of the image
area :: Image -> Int
area _ = 0
