-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Projection is often used in 3D engine. They’re a lot of types of
-- projection. Those supported are contained in 'Projection'.
----------------------------------------------------------------------------

module Photon.Core.Projection (
    -- * Projection
    Projection(..)
  , projectionMatrix
  ) where

import Linear

-- |Projection type.
--
-- @Perspective ratio fovy znear zfar@ creates a perspective projection.
data Projection
  = Perspective
      Float
      Float
      Float
      Float
    deriving (Eq,Show)

-- |Turn a `Projection` into a projection 4x4 matrix.
projectionMatrix :: Projection -> M44 Float
projectionMatrix (Perspective ratio fovy znear zfar) =
    perspectiveMatrix ratio fovy znear zfar

-- |Perspective matrix.
perspectiveMatrix :: Float -> Float -> Float -> Float  -> M44 Float
perspectiveMatrix ratio fovy znear zfar =
    V4
      (V4 itanfovyr        0 0     0   )
      (V4         0 itanfovy 0     0   )
      (V4         0        0 inf   (-1))
      (V4         0        0 nfinf 0   )
  where
    itanfovy = 1 / tan (fovy / 2)
    itanfovyr = itanfovy / ratio
    inf       = 1 / (znear - zfar)
    nfinf     = (znear + zfar) * inf
