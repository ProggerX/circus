module Circus.Types where

import Data.Map
import Data.Vector

data Shape
  = Rect Float Float
  | Circ Float
  | Line Float Float Float Float
  | SimpleSquare
  | SimpleCircle
  deriving (Show, Eq)

type Element = [Shape]

type Bank = Map String Element

data PlacedElement = PlacedElement
  { elName :: String,
    elType :: Element,
    elRot :: Int
  }
  deriving (Show, Eq)

type Matrix a = Vector (Vector a)

data GridConfig = GridConfig
  { gridWidth :: Int,
    gridHeight :: Int,
    cellSize :: Float,
    elementSize :: Float
  }
  deriving (Show, Eq)

defaultGrid :: GridConfig
defaultGrid = GridConfig 40 40 50 40

data Direction = U | R | D | L deriving (Show, Eq, Enum, Bounded)

cyclicSucc :: (Eq a, Enum a, Bounded a) => a -> a
cyclicSucc a
  | a == maxBound = minBound
  | otherwise = succ a

data Link = Link
  { p1 :: (Int, Int),
    c1 :: Direction,
    p2 :: (Int, Int),
    c2 :: Direction
  }
  deriving (Show, Eq)

data Drawing = Drawing
  { matrix :: Matrix (Maybe PlacedElement),
    links :: [Link],
    gridConfig :: GridConfig
  }
  deriving (Show)
