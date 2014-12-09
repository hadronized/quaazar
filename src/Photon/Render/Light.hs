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

module Photon.Render.Light (
    -- * GPU-side light
    GPULight(..)
  , gpuLight
  ) where

import Photon.Render.Semantics ( lightCastShadowsSem, lightColorSem
                               , lightPowerSem, lightRadiusSem, lightTypeSem )
import Photon.Render.Shader ( GPUProgram, programSemantic )

newtype GPULight = GPULight { runLight :: GPUProgram -> IO () } deriving (Eq,Show)

gpuLight :: (Monad m) => Light -> m GPULight
gpuLight (Light t col power radius castShadows) = return . GPULight $ \program -> do
  let sem = programSemantic program
  sem lightCastShadowsSem @?= castShadows
  sem lightColorSem @?= col
  sem lightPowerSem @?= power
  sem lightRadiusSem @?= radius
  sem lightTypeSem @?= t
