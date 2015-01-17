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
import Linear
import Photon.Core.Color ( Color )
import Photon.Core.Entity
import Photon.Core.Light ( Light(..) )
import Photon.Core.Projection ( Projection(..), projectionMatrix )
import Photon.Render.GL.Shader ( Uniform, (@=) )
import Photon.Render.GPU

data GPULight = GPULight {
    runLight :: Uniform Color -- ^ color
             -> Uniform Float -- ^ power
             -> Uniform Float -- ^ radius
             -> Uniform (V3 Float) -- ^ position -- TODO: no sense
             -> Uniform [M44 Float] -- ^ proj*views (depth cubemap)
             -> Uniform Float --  1 / radius
             -> Entity
             -> IO ()
  , onlyIfCastShadows :: IO () -> IO ()
  }

instance GPU Light GPULight where
  gpu = gpuLight

gpuLight :: (Monad m) => Light -> m GPULight
gpuLight (Light _ col power radius castShadows)
    | castShadows = return $ GPULight run id
    | otherwise = return $ GPULight run (const $ return ())
  where
    run colorU powerU radiusU posU projViewsU iRadius ent = do
      colorU @= col
      powerU @= power
      radiusU @= radius
      posU @= ent^.entityPosition
      projViewsU @= lightProjViews 0.1 -- TODO: znear should be put in Light
      iRadius @= 1 / radius
    proj znear = projectionMatrix $ Perspective (pi/2) 1 znear radius
    lightProjViews znear = map ((proj znear !*!) . completeM33RotMat . fromQuaternion)
      [
        axisAngle yAxis (-pi/2) * axisAngle zAxis pi -- positive x
      , axisAngle yAxis (pi/2) * axisAngle zAxis pi -- negative x
      , axisAngle xAxis (-pi/2) -- positive y
      , axisAngle xAxis (pi/2) -- negative y
      , axisAngle yAxis pi * axisAngle zAxis pi -- positive z
      , axisAngle zAxis (pi) -- negative z
      ]

completeM33RotMat :: M33 Float -> M44 Float
completeM33RotMat (V3 (V3 a b c) (V3 d e f) (V3 g h i)) =
  V4
    (V4 a b c 0)
    (V4 d e f 0)
    (V4 g h i 0)
    (V4 0 0 0 1)
