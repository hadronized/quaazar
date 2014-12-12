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
import Linear.V3 ( V3 )
import Photon.Core.Color ( unColor )
import Photon.Core.Entity ( Entity, entityPosition )
import Photon.Core.Light ( Light(..) )
import Photon.Render.GL.Shader ( Uniform, (@=) )

newtype GPULight = GPULight {
    runLight :: Uniform (V3 Float)
             -> Uniform Float
             -> Uniform Float
             -> Uniform (V3 Float)
             -> Entity
             -> IO ()
  }

gpuLight :: (Monad m) => Light -> m GPULight
gpuLight (Light _ col power radius castShadows) =
  return . GPULight $ \coloru poweru radiusu posu ent -> do
  coloru @= unColor col
  poweru @= power
  radiusu @= radius
  posu @= (ent^.entityPosition)
