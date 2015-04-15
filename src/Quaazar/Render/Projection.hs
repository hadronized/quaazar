-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module Quaazar.Render.Projection (
    -- * Projection
    Projection(..)
  , projectionMatrix
  , projectionZFar
  ) where

import Linear

-- |Projection type.
--
-- @Perspective fovy ratio znear zfar@ creates a perspective projection.
data Projection
  = Perspective
      Float
      Float
      Float
      Float
    deriving (Eq,Show)

-- |Turn a `Projection` into a projection 4x4 matrix.
projectionMatrix :: Projection -> M44 Float
projectionMatrix (Perspective fovy ratio znear zfar) =
    perspectiveMatrix fovy ratio znear zfar

-- |Perspective matrix.
perspectiveMatrix :: Float -> Float -> Float -> Float  -> M44 Float
perspectiveMatrix = perspective

projectionZFar :: Projection -> Float
projectionZFar (Perspective _ _ _ zfar) = zfar
