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
import Control.Monad.Trans.State ( evalStateT )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW ( Window, swapBuffers )
import Numeric.Natural ( Natural )
import Quaazar.Render.Compositing
import Quaazar.Render.GL.Framebuffer
import Quaazar.Render.GL.Shader
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Texture
import Quaazar.Render.GL.VertexArray ( VertexArray, bindVertexArray
                                     , genAttributelessVertexArray )
import Quaazar.Render.Light
import Quaazar.Render.Lighting ( Lighting, getLighting, lightOmniBuffer
                               , shadows )
import Quaazar.Render.RenderLayer ( Layer, layerUniform )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

getRenderer :: (Applicative m,MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m,MonadIO q)
            => Float
            -> Float
            -> Natural
            -> Natural
            -> Natural
            -> Maybe ShadowConf
            -> Natural
            -> Window
            -> m (a -> Compositor a Layer -> q())
getRenderer znear zfar w h lightMaxNb shadowConf layerMaxNb window = do
  copyProg <- getCopyProgram
  lighting <- getLighting znear zfar w h lightMaxNb shadowConf
  va <- genAttributelessVertexArray
  compositingOff <- genOffscreenArray w h layerMaxNb Nearest RGBA32F
    (ColorAttachment 0) Depth32F DepthAttachment
  pure $ display copyProg lighting va compositingOff window

getCopyProgram :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
               => m Program
getCopyProgram = buildProgram copyVS Nothing Nothing copyFS

display :: (MonadIO m)
        => Program
        -> Lighting
        -> VertexArray
        -> OffscreenArray
        -> Window
        -> a
        -> Compositor a Layer
        -> m ()
display copyProg lighting va compositing window a compt = liftIO $ do
  bindFramebuffer (compositing^.offscreenArrayFB) ReadWrite
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  layer <- flip evalStateT 0 $ runCompositor compt compositing va 
    (lighting^.lightOmniBuffer) (lighting^.shadows) a
  useProgram copyProg
  unbindFramebuffer ReadWrite
  glClearColor 0 1 0 0
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  bindVertexArray va
  layerUniform @= layer
  compositingColormapsUniform @= (compositing^.offscreenArrayColormaps,Unit 0)
  compositingDepthmapsUniform @= (compositing^.offscreenArrayDepthmaps,Unit 1)
  glDrawArrays gl_TRIANGLE_STRIP 0 4
  liftIO $ swapBuffers window
