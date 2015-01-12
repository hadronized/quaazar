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

module Photon.Render.Forward.IO where

import Control.Lens
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Photon.Render.Forward.Accumulation
import Photon.Render.Forward.Lighting
import Photon.Render.Forward.Post ( Post(..) )
import Photon.Render.Forward.Shadowing
import Photon.Render.Forward.Viewport ( Viewport(..) )
import Photon.Render.Frame ( GPUFrame(..) )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Shader ( useProgram )
import Photon.Render.GL.Texture ( bindTextureAt )
import Photon.Render.GL.VertexArray ( bindVertexArray )

swapBuffers :: Lighting -> Shadowing -> Accumulation -> GPUFrame -> Viewport -> IO ()
swapBuffers lighting shadowing accumulation gpuframe (Viewport w h x y pst) = do
  glDisable gl_BLEND
  bindVertexArray (accumulation^.accumVA)
  (finalOff,_) <- unPost pst lighting shadowing accumulation
  useProgram (accumulation^.accumProgram)
  useFrame gpuframe
  glViewport (fromIntegral w) (fromIntegral h) (fromIntegral x) (fromIntegral y)
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT
  bindTextureAt (finalOff^.offscreenTex) 0
  bindVertexArray (accumulation^.accumVA)
  glDrawArrays gl_TRIANGLE_STRIP 0 4
