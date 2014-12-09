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

module Photon.Render.PostFX (
  ) where

import Photon.Render.GL.Shader ( ShaderType(..), genShader, genProgram
                               , useProgram )
import Photon.Render.GL.VertexArray ( bindVertexArray, genVertexArray
                                    , unbindVertexArray )
import Photon.Render.Texture ( GPUTexture(..) )

newtype PostFX = PostFX String deriving (Eq,Show)

newtype GPUPostFXScreen = GPUPostFXScreen { runPostFXScreen :: IO () }

gpuPostFXScreen :: IO GPUPostFXScreen
gpuPostFXScreen = do
  va <- genVertexArray
  bindVertexArray va
  unbindVertexArray
  return $ do
    bindVertexArray va
    glDrawArrays gl_TRIANGLE_STRIP 0 4
  
newtype GPUPostFX = GPUPostFX { runPostFX :: GPUScreen -> GPUTexture -> IO () }

gpuPostFX :: PostFX -> IO GPUPostFX
gpuPostFX (PostFX src) = do
    program <- sequence [genShader (VertexShader,vsSrc),genShader (FragmentShader,src)] >>= genProgram
    return $ \screen texture -> do
      useProgram program
      bindTextureAt texture 0
      runPostFXScreen screen
