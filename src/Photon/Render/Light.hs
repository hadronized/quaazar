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
import Photon.Core.Color ( unColor )
import Photon.Core.Entity ( Entity, entityPosition )
import Photon.Core.Light ( Light(..) )
import Photon.Render.GL.Shader ( Uniform, Uniformable, (@?=) )
import Photon.Render.Semantics ( lightCastShadowsSem, lightColorSem
                               , lightPositionSem, lightPowerSem, lightRadiusSem
                               , lightTypeSem )
import Photon.Render.Shader ( GPUProgram, programSemantic )

newtype GPULight = GPULight { runLight :: GPUProgram -> Entity -> IO () }

gpuLight :: (Monad m) => Light -> m GPULight
gpuLight (Light t col power radius castShadows) = return . GPULight $ \program ent -> do
  let
    sem :: (Uniformable a) => Int -> Maybe (Uniform a)
    sem = programSemantic program
  sem lightCastShadowsSem @?= fromBool castShadows
  sem lightColorSem @?= unColor col
  sem lightPowerSem @?= power
  sem lightRadiusSem @?= radius
  --sem lightTypeSem @?= t -- FIXME
  sem lightPositionSem @?= (ent^.entityPosition)
    
fromBool :: Bool -> Int
fromBool b = if b then 1 else 0
