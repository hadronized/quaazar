-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Photon.Render.Material (
    -- * GPU-side material
    GPUMaterial(..)
  , gpuMaterial
  ) where

import Linear.V3 ( V3 )
import Photon.Core.Material ( Material(..), MaterialLayer(..), unAlbedo )
import Photon.Render.GL.Shader ( Uniform, (@=) )

newtype GPUMaterial = GPUMaterial {
    runMaterial :: Uniform (V3 Float) -- ^ diffuse albedo
                -> Uniform (V3 Float) -- ^ specular albedo
                -> Uniform Float -- ^ shininess
                -> IO ()
  }

-- TODO: implement multilayered material
gpuMaterial :: (Monad m) => Material -> m GPUMaterial
gpuMaterial (Material []) = return . GPUMaterial $ \_ _ _ -> return ()
gpuMaterial (Material (MaterialLayer dalb salb shn:_)) =
  return . GPUMaterial $ \diffu specu shnu -> do
    diffu @= unAlbedo dalb
    specu @= unAlbedo salb
    shnu @= shn
