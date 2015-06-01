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

module Quaazar.Render.Renderer where

import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW ( Window, swapBuffers )
import Numeric.Natural ( Natural )
import Quaazar.Render.Compositing
import Quaazar.Render.GL.Framebuffer
import Quaazar.Render.GL.Shader
import Quaazar.Render.GL.Primitive
import Quaazar.Render.GL.Texture
import Quaazar.Render.GL.VertexArray
import Quaazar.Render.Light
import Quaazar.Render.Lighting ( Lighting, getLighting, lightOmniBuffer
                               , shadows )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

getRenderer :: (Applicative m,MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m,MonadIO q)
            => Float
            -> Float
            -> Natural
            -> Natural
            -> Natural
            -> Maybe ShadowConf
            -> Window
            -> m (a -> Compositor a Texture2D -> q())
getRenderer znear zfar w h lightMaxNb shadowConf window = do
  copyProg <- getCopyProgram
  lighting <- getLighting znear zfar w h lightMaxNb shadowConf
  va <- genAttributelessVertexArray
  pure $ display copyProg lighting va window

getCopyProgram :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
               => m Program
getCopyProgram = buildProgram copyVS Nothing Nothing copyFS

display :: (MonadIO m)
        => Program
        -> Lighting
        -> VertexArray
        -> Window
        -> a
        -> Compositor a Texture2D
        -> m ()
display copyProg lighting va window a compt = liftIO $ do
  image <- runCompositor compt va  (lighting^.lightOmniBuffer)
    (lighting^.shadows) a
  useProgram copyProg
  unbindFramebuffer ReadWrite
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  colormapUniform @= (image,Unit 0)
  bindVertexArray va
  drawArrays STriangle 0 4
  liftIO $ swapBuffers window
