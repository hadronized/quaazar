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

module Quaazar.Scene.BoundingVolume (
    -- * Bounding volumes
    BoundingVolume(..)
  , bsphere
  ) where

import Control.Lens ( view )
import Linear
import Quaazar.Geometry.Position ( unPosition )
import Quaazar.Geometry.Vertex ( Vertices(..), vertexPosition )

data BoundingVolume
  = BSphere Float          -- ^ radius
  | AABB Float Float Float -- ^ width, height and depth
    deriving (Eq,Show)

-- |Construct a bounding sphere from vertices.
bsphere :: Vertices -> BoundingVolume
bsphere verts = BSphere $ case verts of
    Interleaved vs         -> maximum $ map (norm . unPosition . view vertexPosition) vs
    Deinterleaved _ ps _ _ -> maximum $ map (norm . unPosition) ps

{-
minmax :: (Ord a) => [a] -> (a,a)
minmax []     = error "empty list: minmax"
minmax (x:xs) = foldl (\(mn,mx) a -> (min mn a,max mx a)) (x,x) xs
-}
