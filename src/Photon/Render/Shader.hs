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

import Control.Lens ( makeLenses )
import Data.Vector ( Vector, (!) )
import Graphics.Rendering.OpenGL.Raw
import Photon.Render.GL.Shader ( Uniform, genProgram, genShader
                               , getUniformLocation, uniform )
import qualified Photon.Render.GL.Shader as GL ( useProgram )

data GPUProgram = GPUProgram {
    _gpuShaderProgram   :: Program
  , _gpuShaderSemantics :: Vector GLInt
  }

makeLenses ''GPUProgram

gpuProgram :: [(ShaderType,String)] -> [String] -> IO GPUProgram
gpuProgram shaders semantics = do
  program <- mapM (uncurry genShader) shaders >>= genProgram
  semantics' <- mapM (getUniformLocation program) semantics)
  return (GPUProgram program semantics')

programSemantic :: GPUProgram -> Int -> Maybe (Uniform a)
programSemantic (GPUProgram semantics _) sem = fmap uniform (semantics !? sem)

useProgram :: GPUProgram -> IO ()
useProgram (GPUProgram p _) = GL.useProgram p
