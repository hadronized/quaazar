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

module Photon.Render.PostFX where

import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Graphics.Rendering.OpenGL.Raw
import Photon.Render.GL.Shader ( ShaderType(..), genShader, genProgram
                               , useProgram )
import Photon.Render.GL.VertexArray ( bindVertexArray, genVertexArray
                                    , unbindVertexArray )
import Photon.Render.Texture ( GPUTexture(..) )
import Photon.Utils.Log ( MonadLogger )

newtype PostFX = PostFX String deriving (Eq,Show)

newtype GPUPostFXScreen = GPUPostFXScreen { runPostFXScreen :: IO () }

gpuPostFXScreen :: (MonadIO m) => m GPUPostFXScreen
gpuPostFXScreen = liftIO $ do
  va <- genVertexArray
  bindVertexArray va
  unbindVertexArray
  return . GPUPostFXScreen $ do
    bindVertexArray va
    glDrawArrays gl_TRIANGLE_STRIP 0 4
  
newtype GPUPostFX = GPUPostFX { runPostFX :: GPUPostFXScreen -> GPUTexture -> IO () }

gpuPostFX :: (MonadIO m,MonadLogger m,MonadError String m) => PostFX -> m GPUPostFX
gpuPostFX (PostFX src) = do
    program <- sequence [genShader VertexShader vsSrc,genShader FragmentShader src] >>= genProgram
    return . GPUPostFX $ \screen texture -> do
      useProgram program
      bindTextureAt texture 0
      runPostFXScreen screen

vsSrc :: String
vsSrc = unlines
    [
    "#version 330 core"
  , "out vec2 uv;"
  , "vec2[4] v = vec2[]("
  , "    vec2(-1,  1)"
  , "  , vec2( 1,  1)"
  , "  , vec2(-1, -1)"
  , "  , vec2( 1, -1)"
  , "  );"
  , "void main() {"
  , "  uv = (v[gl_VertexID] + 1) * 0.5;"
  , "  gl_Position = vec4(v[gl_VertexID], 0., 1.);"
  , "}"
  ]
