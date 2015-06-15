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

module Quaazar.System.Managers (
    -- * All resource managers
    getResourceManagers
  ) where

import Numeric.Natural ( Natural )
import Quaazar.Geometry.Mesh
import Quaazar.Lighting.Light
import Quaazar.Render.GL.Texture
import Quaazar.System.Resource
import Quaazar.Technics.Lighting.Phong

getResourceManagers :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
                    => m (
                         String -> m Mesh
                       , String -> m Ambient
                       , String -> m Omni
                       , String -> Wrap -> Filter -> Maybe CompareFunc -> Natural -> Natural -> m Texture2D
                       , String -> m PhongMaterial
                       )
getResourceManagers = do
  m <- getMeshManager
  amb <- getAmbientManager
  omni <- getOmniManager
  tex2D <- getTexture2DManager
  phongMat <- getPhongMaterialManager tex2D
  pure (m,amb,omni,tex2D,phongMat)
  
