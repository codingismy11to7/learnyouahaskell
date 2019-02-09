module Geometry.Cuboid
  ( volume
  , area
  ) where

volume :: (Num x) => x -> x -> x -> x
volume a b c = rectangleArea a b * c

area :: (Num x) => x -> x -> x -> x
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: (Num x) => x -> x -> x
rectangleArea a b = a * b
