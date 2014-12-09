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

module Photon.Render.Shader (
    -- * GPU-side program
  ) where

import Graphics.Rendering.
data GPUProgram = GPUProgram {
    _gpuShaderProgram   :: Program
  , _gpuShaderSemantics :: Vector GLInt
  }

gpuProgram :: Program -> [String] -> IO GPUProgram
gpuProgram p semantics = fmap (GPU Program) (mapM getUniformLocation semantics)
