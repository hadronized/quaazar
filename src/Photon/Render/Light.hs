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
import Photon.Core.Color ( Color )
import Photon.Core.Entity ( Entity, entityPosition )
import Photon.Core.Light ( Light(..) )
import Photon.Render.GL.Entity ( cameraTransform )
import Photon.Render.GL.Shader ( Uniform, (@=) )
import Photon.Render.GPU

data GPULight = GPULight {
    shadeWithLight :: Uniform Color -- ^ color
                   -> Uniform Float -- ^ power
                   -> Uniform Float -- ^ radius
                   -> Uniform (V3 Float) -- ^ position -- TODO: no sense
                   -> Uniform (M44 Float) -- projection * view
                   -> Entity
                   -> IO ()
  , genDepthmap :: IO () -> IO ()
  , lightRadius :: Float -- TODO: c’est le bordel ça !
  }

instance GPU Light GPULight where
  gpu = gpuLight

gpuLight :: (Monad m) => Light -> m GPULight
gpuLight (Light _ col power radius castShadows) =
    return (GPULight sendProperties sendDepthmap radius)
  where
    sendProperties colorU powerU radiusU posU projViewU ent = do
      colorU @= col
      powerU @= power
      radiusU @= radius
      posU @= (ent^.entityPosition)
      projViewU @= cameraTransform ent
    sendDepthmap
      | castShadows = id
      | otherwise = const (return ())
