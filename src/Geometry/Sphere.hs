module Geometry.Sphere
  ( volume
  , area
  ) where

volume :: (Floating x) => x -> x
volume radius = 4 / 3 * pi * radius ^ 3

area :: (Floating x) => x -> x
area radius = 4 * pi * radius ^ 2
