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

import Photon.Core.Material ( Material )
import Photon.Render.Shader ( GPUShader )

newtype GPUMaterial = GPUMaterial { runMaterial :: GPUShader -> IO () } deriving (Eq,Show)

gpuMaterial :: Material -> IO GPUMaterial
gpuMaterial (Material dalb salb shn) = return . GPUMaterial $ \program -> do
  case mapM (programSemantic program) materialSemantics of
    Just [dalbSem,salbSem,shnSem] -> do
      dalbSem @= unAlbedo dalb
      salbSem @= unAlbedo salb
      shnSem @= shn
    Nothing -> return ()
