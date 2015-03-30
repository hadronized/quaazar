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

module Quaazar.Render.Frame where

import Control.Lens
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Error.Class ( MonadError )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import qualified Graphics.UI.GLFW as GLFW ( swapBuffers )
import Numeric.Natural ( Natural )
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Renderer
import Quaazar.Render.Forward.RenderLayer ( RenderLayer(..) )
import Quaazar.Render.Forward.Viewport ( Viewport(..), setViewport )
import Quaazar.Render.GL.Framebuffer (Target(..), bindFramebuffer
                                     , unbindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( useProgram )
import Quaazar.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..)
                                 , bindTextureAt )
import Quaazar.Render.GL.VertexArray ( bindVertexArray )
import Quaazar.Render.Texture ( GPUTexture(GPUTexture) )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data GPUFrame = GPUFrame {
    useFrame :: IO ()
  , colormap :: GPUTexture
  , depthmap :: GPUTexture
  }

screenFrame :: GPUFrame
screenFrame =
  GPUFrame
    (unbindFramebuffer ReadWrite)
    (error "screen frame colormap")
    (error "screen frame depthmap")

gpuFrame :: (MonadScoped IO m, MonadIO m,MonadError Log m)
         => Natural
         -> Natural
         -> m GPUFrame
gpuFrame w h = do
  off <- genOffscreen w h Nearest RGB32F RGB
  return $
    GPUFrame
      (bindFramebuffer (off^.offscreenFB) ReadWrite)
      (GPUTexture . bindTextureAt $ off^.offscreenRender)
      (GPUTexture . bindTextureAt $ off^.offscreenDepthmap)

displayInto :: (MonadIO m) => ForwardRenderer -> GPUFrame -> Viewport -> RenderLayer -> m ()
displayInto (ForwardRenderer lighting accumulation _) gpuframe screenViewport pst = liftIO $ do
  setViewport screenViewport
  (finalOff,_) <- unRenderLayer pst lighting accumulation
  useProgram (accumulation^.accumProgram)
  useFrame gpuframe
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT
  bindTextureAt (finalOff^.offscreenRender) 0
  bindVertexArray (accumulation^.accumVA)
  glDrawArrays gl_TRIANGLE_STRIP 0 4

swapBuffers :: (MonadIO m) => ForwardRenderer -> m ()
swapBuffers fr = liftIO $ GLFW.swapBuffers (fr^.frWindow)
