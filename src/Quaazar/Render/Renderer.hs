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
import Quaazar.Render.Compositing ( Compositor(..), copyVS, copyFS )
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

data Renderer = Renderer {
    _frCopyProgram :: Program
  , _frLighting    :: Lighting
  , _frVA          :: VertexArray
  , _frOff         :: OffscreenArray
  , _frWindow      :: Window
  }

makeLenses ''Renderer

getRenderer :: (Applicative m,MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
            => Float
            -> Float
            -> Natural
            -> Natural
            -> Natural
            -> Maybe ShadowConf
            -> Natural
            -> Window
            -> m Renderer
getRenderer znear zfar w h lightMaxNb shadowConf layerMaxNb window =
  Renderer
    <$> getCopyProgram
    <*> getLighting znear zfar w h lightMaxNb shadowConf
    <*> genAttributelessVertexArray
    <*> genOffscreenArray w h layerMaxNb Nearest RGBA32F (ColorAttachment 0)
          Depth32F DepthAttachment
    <*> pure window

getCopyProgram :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
               => m Program
getCopyProgram = buildProgram copyVS Nothing Nothing copyFS

display :: (MonadIO m) => Renderer -> a -> Compositor a Layer -> m ()
display rdr a compt = liftIO $ do
  layer <- flip evalStateT 0 $ runCompositor compt
    (rdr^.frOff) (rdr^.frVA) (rdr^.frLighting.lightOmniBuffer)
    (rdr^.frLighting.shadows) a
  useProgram (rdr^.frCopyProgram)
  unbindFramebuffer ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  bindVertexArray (rdr^.frVA)
  layerUniform @= layer
  glDrawArrays gl_TRIANGLE_STRIP 0 4
  liftIO $ swapBuffers (rdr^.frWindow)
