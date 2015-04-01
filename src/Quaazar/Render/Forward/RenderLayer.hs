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

module Quaazar.Render.Forward.RenderLayer where

import Control.Lens
import Graphics.Rendering.OpenGL.Raw
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Looked ( Looked(..) )
import Quaazar.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.VertexArray ( bindVertexArray )
import Quaazar.Render.PostFX ( GPUPostFX(..) )

newtype RenderLayer = RenderLayer {
    unRenderLayer :: Lighting -> Accumulation -> IO PingPong
  }

type PingPong = (Offscreen,Offscreen)

renderLayer :: Looked -> RenderLayer
renderLayer lk = RenderLayer fromLooked
  where
    fromLooked lighting accumulation = do
      purgeAccumulationFramebuffer accumulation
      unLooked lk lighting accumulation
      glDisable gl_BLEND
      bindVertexArray (accumulation^.accumVA)
      return (lighting^.lightOff,accumulation^.accumOff)

post :: GPUPostFX a -> a -> RenderLayer -> RenderLayer
post  gpupfx a prev = RenderLayer post_
  where
    post_ lighting accumulation = do
      (sourceOff,targetOff) <- unRenderLayer prev lighting accumulation
      usePostFX gpupfx (sourceOff^.offscreenRender) (sourceOff^.offscreenDepthmap) a
      bindFramebuffer (targetOff^.offscreenFB) ReadWrite
      glClear gl_DEPTH_BUFFER_BIT
      glDrawArrays gl_TRIANGLE_STRIP 0 4
      return (targetOff,sourceOff) -- pingpong ! \o/
