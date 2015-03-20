{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Render.PostFX where

import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Quaazar.Core.PostFX
import Quaazar.Render.GL.Texture
import Quaazar.Render.Shader
import Quaazar.Utils.Log ( Log, MonadLogger )
import Quaazar.Utils.Scoped

data GPUPostFX a = GPUPostFX { usePostFX :: Texture2D -> a -> IO () }

gpuPostFX :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
          => PostFX
          -> (a -> Semantics b)
          -> m (GPUPostFX a)
gpuPostFX (PostFX src) semMapper = do
    gpuprogram <- gpuProgram vsSrc Nothing Nothing src semMapper
    return $ GPUPostFX (use gpuprogram)
  where
    use gprog sourceTex a = do
      bindTextureAt sourceTex 0
      useProgram gprog
      sendToProgram gprog a

gpuPostFXFree :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
              => PostFX
              -> m (GPUPostFX ())
gpuPostFXFree pfx = gpuPostFX pfx . const $ return ()

vsSrc :: String
vsSrc = unlines
    [
    "#version 430 core"
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
