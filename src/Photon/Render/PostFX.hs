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
import Photon.Core.PostFX
import Photon.Render.GL.GLObject
import Photon.Render.GL.Shader
import Photon.Render.GL.Texture
import Photon.Utils.Log ( Log, MonadLogger, sinkLogs )

newtype GPUPostFX = GPUPostFX { usePostFX :: Texture2D -> IO () }

gpuPostFX :: (MonadIO m,MonadLogger m,MonadError Log m) => PostFX -> m GPUPostFX
gpuPostFX (PostFX src) = do
    vs <- liftIO (genObject :: IO VertexShader)
    fs <- liftIO (genObject :: IO FragmentShader)
    program <- liftIO genObject
    sequence_ [compile vs vsSrc,compile fs src]
    sequence_ [attach program vs,attach program fs]
    link program
    liftIO $ do
      deleteObject vs
      deleteObject fs
      sourceTexU <- getUniform program "sourceTex"
      useProgram program
      sourceTexU @= (0 :: Int)
    sinkLogs
    return . GPUPostFX $ \sourceTex -> do
      useProgram program
      bindTextureAt sourceTex 0

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
