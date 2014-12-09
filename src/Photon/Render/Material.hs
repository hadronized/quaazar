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

import Photon.Core.Material ( MaterialLayer(..), unAlbedo )
import Photon.Render.GL.Shader ( Uniform, Uniformable, (@?=) )
import Photon.Render.Semantics ( materialDiffuseAlbedoSem
                               , materialShininessSem
                               , materialSpecularAlbedoSem )
import Photon.Render.Shader ( GPUProgram, programSemantic )

newtype GPUMaterial = GPUMaterial { runMaterial :: GPUProgram -> IO () }

gpuMaterial :: (Monad m) => MaterialLayer -> m GPUMaterial
gpuMaterial (MaterialLayer dalb salb shn) = return . GPUMaterial $ \program -> do
   let
     sem :: (Uniformable a) => Int -> Maybe (Uniform a)
     sem = programSemantic program
   sem materialDiffuseAlbedoSem @?= unAlbedo dalb
   sem materialSpecularAlbedoSem @?= unAlbedo salb
   sem materialShininessSem @?= shn
