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

module Quaazar.Render.Material (
    -- * GPU-side material
    GPUMaterial(..)
  , gpuMaterial
  ) where

import Quaazar.Core.Material ( Albedo, Material(..) )
import Quaazar.Render.GL.Shader ( Uniform, (@=) )

newtype GPUMaterial = GPUMaterial {
    runMaterial :: Uniform Albedo -- ^ diffuse albedo
                -> Uniform Albedo -- ^ specular albedo
                -> Uniform Float -- ^ shininess
                -> IO ()
  }

-- TODO: implement multilayered material
gpuMaterial :: (Monad m) => Material -> m GPUMaterial
gpuMaterial (Material dalb salb shn) =
  return . GPUMaterial $ \diffu specu shnu -> do
    diffu @= dalb
    specu @= salb
    shnu @= shn
