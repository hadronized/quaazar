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

module Photon.Render.Light where

import Control.Lens
import Linear.Matrix ( M44 )
import Linear.V3 ( V3 )
import Photon.Core.Color ( unColor )
import Photon.Core.Entity ( Entity, entityPosition )
import Photon.Core.Light ( Light(..) )
import Photon.Render.GL.Entity ( cameraTransform )
import Photon.Render.GL.Shader ( Uniform, (@=) )

data GPULight = GPULight {
    shadeWithLight :: Uniform (V3 Float) -- ^ color
                   -> Uniform Float -- ^ power
                   -> Uniform Float -- ^ radius
                   -> Uniform (V3 Float) -- ^ position
                   -> Uniform (M44 Float) -- projection * view
                   -> Entity
                   -> IO ()
  , genDepthmap :: IO () -> IO ()
  }

gpuLight :: (Monad m) => Light -> m GPULight
gpuLight (Light _ col power radius castShadows) =
    return (GPULight sendProperties sendDepthmap)
  where
    sendProperties colorU powerU radiusU posU projViewU ent = do
      colorU @= unColor col
      powerU @= power
      radiusU @= radius
      posU @= (ent^.entityPosition)
      projViewU @= cameraTransform ent
    sendDepthmap
      | castShadows = id
      | otherwise = const (return ())
