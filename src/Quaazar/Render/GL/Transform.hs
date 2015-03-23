-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Render.GL.Transform (
    -- *
    transformMatrix
  , cameraMatrix
  ) where

import Control.Lens
import Linear
import Quaazar.Core.Transform

transformMatrix :: Transform -> M44 Float
transformMatrix e = mkTransformation o p !*! scaled (V4 sx sy sz 1)
  where
    p = e^.transformPosition
    o = e^.transformOrientation
    Scale sx sy sz = e^.transformScale

-- FIXME: cameraTransform is also used with lights; bad name!
cameraMatrix :: Transform -> M44 Float
cameraMatrix e = quaternionMatrix o !*! translationMatrix p
  where
    p = negate (e^.transformPosition)
    o = e^.transformOrientation

translationMatrix :: (Num a) => V3 a -> M44 a
translationMatrix (V3 x y z) =
    V4
      (V4 1 0 0 x)
      (V4 0 1 0 y)
      (V4 0 0 1 z)
      (V4 0 0 0 1)

quaternionMatrix :: (Num a) => Quaternion a -> M44 a
quaternionMatrix q =
    V4
      (fix4 rx)
      (fix4 ry)
      (fix4 rz)
      (V4 0 0 0 1)
  where
    V3 rx ry rz = fromQuaternion q
    fix4 (V3 x y z) = V4 x y z 0
