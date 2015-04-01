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

module Quaazar.Render.Forward.Renderer where

import Control.Applicative
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW ( Window, swapBuffers )
import Numeric.Natural ( Natural )
import Quaazar.Render.Compositing ( Compositor(..), copyVS, copyFS )
import Quaazar.Render.Forward.Accumulation ( Accumulation, getAccumulation )
import Quaazar.Render.Forward.Lighting ( Lighting, getLighting
                                       , lightOmniBuffer )
import Quaazar.Render.GL.Framebuffer ( Target(ReadWrite), unbindFramebuffer )
import Quaazar.Render.GL.Shader ( Program, buildProgram, useProgram )
import Quaazar.Render.GL.VertexArray ( VertexArray, bindVertexArray
                                     , genAttributelessVertexArray )
import Quaazar.Render.Texture ( GPUTexture(..) )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data ForwardRenderer = ForwardRenderer {
    _frCopyProgram :: Program
  , _frLighting    :: Lighting
  , _frAccumulation :: Accumulation
  , _frVA          :: VertexArray
  , _frWindow      :: Window
  }

makeLenses ''ForwardRenderer

getRenderer :: (Applicative m,MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
            => Natural
            -> Natural
            -> Natural
            -> Natural
            -> Window
            -> m ForwardRenderer
getRenderer w h shadowDef lightNb window =
  ForwardRenderer
    <$> getCopyProgram
    <*> getLighting w h lightNb
    <*> getAccumulation w h
    <*> genAttributelessVertexArray
    <*> pure window

getCopyProgram :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
               => m Program
getCopyProgram = buildProgram copyVS Nothing Nothing copyFS

display :: (MonadIO m) => ForwardRenderer -> a -> Compositor a GPUTexture -> m ()
display rdr a compt = liftIO $ do
  source <- runCompositor compt (rdr^.frVA) (rdr^.frLighting.lightOmniBuffer) a
  useProgram (rdr^.frCopyProgram)
  unbindFramebuffer ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  bindVertexArray (rdr^.frVA)
  glDrawArrays gl_TRIANGLE_STRIP 0 4
  bindTextureAt source 0
  liftIO $ swapBuffers (rdr^.frWindow)
