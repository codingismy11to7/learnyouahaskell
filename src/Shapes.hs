module Shapes
  ( Point(..)
  , Shape(..)
  , surface
  , nudge
  , baseCircle
  , baseRectangle
  ) where

data Point = Point
  { x :: Double
  , y :: Double
  } deriving (Show, Eq)

data Shape
  = Circle { origin :: Point
           , radius :: Double }
  | Rectangle { topLeft  :: Point
              , btmRight :: Point }
  deriving (Show, Eq)

surface :: Shape -> Double
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

nudge :: Shape -> Double -> Double -> Shape
nudge (Circle (Point x y) r) deltX deltY = Circle (Point (x + deltX) (y + deltY)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) deltX deltY =
  Rectangle (Point (x1 + deltX) (y1 + deltY)) (Point (x2 + deltX) (y2 + deltY))

baseCircle :: Double -> Shape
baseCircle = Circle (Point 0 0)

baseRectangle :: Double -> Double -> Shape
baseRectangle w h = Rectangle (Point 0 0) (Point w h)
