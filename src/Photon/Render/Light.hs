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

newtype GPULight = GPULight { unGPULight :: Light } deriving (Eq,Show)

gpuLight :: Light -> GPULight
gpuLight = return . GPULight
