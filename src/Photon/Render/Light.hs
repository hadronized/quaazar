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
    shadeWithLight :: Uniform Color -- ^ color
                   -> Uniform Float -- ^ power
                   -> Uniform Float -- ^ radius
                   -> Uniform (V3 Float) -- ^ position -- TODO: no sense
                   -> Entity
                   -> IO ()
  , genDepthmap    :: Uniform [M44 Float] -- ^ light proj*views
                   -> Uniform (V3 Float) -- ^ position
                   -> Uniform Float -- ^ 1 / radius
                   -> Entity
                   -> Float -- znear
                   -> IO ()
  }

instance GPU Light GPULight where
  gpu = gpuLight

gpuLight :: (Monad m) => Light -> m GPULight
gpuLight (Light _ col power radius castShadows) =
    return $ GPULight shading depthmap'
  where
    shading colorU powerU radiusU posU ent = do
      colorU @= col
      powerU @= power
      radiusU @= radius
      posU @= ent^.entityPosition
    depthmap ligProjViewsU ligPosU ligIRadU ent znear = do
      ligProjViewsU @= lightProjViews znear
      ligPosU @= (ent^.entityPosition)
      ligIRadU @= 1 / radius
    noDepthmap _ _ _ _ _ = return ()
    depthmap'
      | castShadows = depthmap
      | otherwise = noDepthmap
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
