module Geometry.Cube
  ( area
  , volume
  ) where

import qualified Geometry.Cuboid as Cuboid

volume side = Cuboid.volume side side side

area side = Cuboid.area side side side
