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

module Quaazar.Render.Forward.IO where

import Control.Lens
import Control.Monad.Trans ( MonadIO(..) )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.GLFW as GLFW ( swapBuffers )
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Post ( Post(..) )
import Quaazar.Render.Forward.Renderer
import Quaazar.Render.Forward.Viewport ( Viewport(..), setViewport )
import Quaazar.Render.Frame ( GPUFrame(..) )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( useProgram )
import Quaazar.Render.GL.Texture ( bindTextureAt )
import Quaazar.Render.GL.VertexArray ( bindVertexArray )

displayInto :: (MonadIO m) => ForwardRenderer -> GPUFrame -> Viewport -> Post -> m ()
displayInto (ForwardRenderer lighting shadowing accumulation _) gpuframe screenViewport pst = liftIO $ do
  setViewport screenViewport
  (finalOff,_) <- unPost pst lighting shadowing accumulation
  useProgram (accumulation^.accumProgram)
  useFrame gpuframe
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT
  bindTextureAt (finalOff^.offscreenRender) 0
  bindVertexArray (accumulation^.accumVA)
  glDrawArrays gl_TRIANGLE_STRIP 0 4

swapBuffers :: (MonadIO m) => ForwardRenderer -> m ()
swapBuffers fr = liftIO $ GLFW.swapBuffers (fr^.frWindow)
