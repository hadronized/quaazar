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

module Quaazar.Render.Forward.Rendered where

import Data.Monoid ( Monoid(..) )
import Linear ( M44 )
import Quaazar.Core.Entity ( Entity )
import Quaazar.Core.Material ( Albedo )
import Quaazar.Render.GL.Shader ( Uniform )
import Quaazar.Render.Material ( GPUMaterial(..) )
import Quaazar.Render.Mesh ( GPUMesh(..) )

newtype Rendered = Rendered {
    unRendered :: Uniform (M44 Float) -- ^ model matrix
               -> Uniform Albedo      -- ^ diffuse albedo
               -> Uniform Albedo      -- ^ specular albedo
               -> Uniform Float       -- ^ shininess
               -> IO ()
  }

instance Monoid Rendered where
  mempty = Rendered $ \_ _ _ _ -> return ()
  Rendered f `mappend` Rendered g =
    Rendered $ \m df sp sh -> f m df sp sh >> g m df sp sh

render :: GPUMesh -> GPUMaterial -> Entity -> Rendered
render gmsh gmat ent = Rendered $ \modelU matDiffAlbU matSpecAlbU matShnU -> do
    runMaterial gmat matDiffAlbU matSpecAlbU matShnU
    renderMesh gmsh modelU ent
