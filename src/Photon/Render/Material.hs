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
import Photon.Core.Material ( MaterialLayer(..), unAlbedo )
import Photon.Render.GL.Shader ( Uniform, (@=) )

newtype GPUMaterial = GPUMaterial {
    runMaterial :: Uniform (V3 Float)
                -> Uniform (V3 Float)
                -> Uniform Float
                -> IO ()
  }

gpuMaterial :: (Monad m) => MaterialLayer -> m GPUMaterial
gpuMaterial (MaterialLayer dalb salb shn) =
  return . GPUMaterial $ \diffu specu shnu -> do
    diffu @= unAlbedo dalb
    specu @= unAlbedo salb
    shnu @= shn
