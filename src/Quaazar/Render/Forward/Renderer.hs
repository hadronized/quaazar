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
import Graphics.UI.GLFW ( Window )
import Numeric.Natural ( Natural )
import Quaazar.Render.Compositing ( Compositor, comptVA, copyVS, copyFS
                                  , getCompositor )
import Quaazar.Render.Forward.Accumulation ( Accumulation, getAccumulation )
import Quaazar.Render.Forward.Lighting ( Lighting, getLighting )
import Quaazar.Render.GL.Framebuffer ( Target(ReadWrite), unbindFramebuffer )
import Quaazar.Render.GL.Shader ( Program, buildProgram, useProgram )
import Quaazar.Render.GL.VertexArray ( bindVertexArray )
import Quaazar.Render.Texture ( GPUTexture )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data ForwardRenderer = ForwardRenderer {
    _frCopyProgram :: Program
  , _frLighting    :: Lighting
  , _frAccumulation :: Accumulation
  , _frCompositor  :: Compositor
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
    <*> getCompositor
    <*> pure window

getCopyProgram :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
               => m Program
getCopyProgram = buildProgram copyVS Nothing Nothing copyFS

display :: (MonadIO m) => ForwardRenderer -> GPUTexture -> m ()
display rdr tex = liftIO $ do
  useProgram (rdr^.frCopyProgram)
  unbindFramebuffer ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  bindVertexArray (rdr^.frCompositor.comptVA)
  glDrawArrays gl_TRIANGLE_STRIP 0 4
