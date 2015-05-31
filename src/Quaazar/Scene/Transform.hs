-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Transforms represents spacial information.
--
-- Up to now, supported spatial information is:
--
--   - *position* ;
--   - *orientation* ;
--   - *scaling*.
----------------------------------------------------------------------------

module Quaazar.Scene.Transform (
    -- * Transform
    Transform(Transform)
  , transformPosition
  , transformOrientation
  , transformScale
    -- * Space information
  , Dir
  , Axis
  , Orientation
  , Scale(..)
  , origin3
  , xAxis
  , yAxis
  , zAxis
    -- * Combinators
  , noScale
  , uniScale
  , move
  , position
  , orient
  , orientation
  , rescale
  , scale
  , transformation
  ) where

import Control.Lens
import Data.Semigroup ( Semigroup(..) )
import Linear

-- |@Scale x y z@ packs a scaling operation applied on a 3D-float vector.
data Scale = Scale !Float !Float !Float deriving (Eq,Ord,Show)

-- |Position in space.
type Position = V3 Float

-- |Direction.
type Dir = V3 Float

-- |Axis. Typically used to build rotation around axis.
type Axis = V3 Float

-- |Local orientation. Implemented with quaternions.
type Orientation = Quaternion Float

-- |A transform is a spatial transformation. So far, entities enable the use of
-- three space properties:
--
--   - position;
--   - orientation;
--   - scaling.
data Transform = Transform {
    -- |Transform’s position.
    _transformPosition    :: Position
    -- |Transform’s orientation.
  , _transformOrientation :: Orientation
    -- |Transform’s scale.
  , _transformScale       :: Scale
  } deriving (Eq,Show)

makeLenses ''Transform

instance Semigroup Transform where
  Transform p0 o0 s0 <> Transform p1 o1 s1 = Transform p2 o2 s2
    where
      p2 = p0 + p1
      o2 = normalize $ o0 * o1
      s2 = Scale (s0x * s1x) (s0y * s1y) (s0z * s1z)
      Scale s0x s0y s0z = s0
      Scale s1x s1y s1z = s1

instance Monoid Transform where
  mempty = Transform origin3 (axisAngle (-zAxis) 0) noScale
  mappend = (<>)

-- |Origin of the R³ basis (0,0,0).
origin3 :: Position
origin3 = V3 0 0 0

-- |Common axis.
xAxis,yAxis,zAxis :: Axis
xAxis = V3 1 0 0
yAxis = V3 0 1 0
zAxis = V3 0 0 1

-- |No scale.
noScale :: Scale
noScale = Scale 1 1 1

-- |Accumulate a transform’s position with another.
move :: Dir -> Transform -> Transform
move dir = transformPosition %~ (+dir)

-- |Reset the position of a transform.
position :: Position -> Transform -> Transform
position = set transformPosition

-- |Accumulate a transform’s orientation with another.
orient :: V3 Float -> Float -> Transform -> Transform
orient axis phi = transformOrientation %~ (normalize . (axisAngle axis phi *))

-- |Reset the orientation of a transform.
orientation :: V3 Float -> Float -> Transform -> Transform
orientation axis phi = set transformOrientation $ axisAngle axis phi 

-- |Accumulate a transform’s scale with another.
rescale :: Scale -> Transform -> Transform
rescale (Scale x' y' z') = transformScale %~ \(Scale x y z) -> Scale (x*x') (y*y') (z*z')

-- |Reset the scale of a transform.
scale :: Scale -> Transform -> Transform
scale = set transformScale

-- |Uniform scale.
uniScale :: Float -> Transform -> Transform
uniScale s = scale $ Scale s s s

-- |Build a new 'Transform' out of a transformation function
-- (@Transform -> Transform@). That function is nice when you want to create a
-- transform which is the composition of several transformations.
transformation :: (Transform -> Transform) -> Transform
transformation = ($ mempty)
