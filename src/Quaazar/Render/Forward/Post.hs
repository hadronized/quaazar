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

module Quaazar.Render.Forward.Post where

import Control.Lens
import Graphics.Rendering.OpenGL.Raw
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Looked ( Looked(..) )
import Quaazar.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.VertexArray ( bindVertexArray )
import Quaazar.Render.PostFX ( GPUPostFX(..) )

newtype Post = Post { unPost :: Lighting -> Accumulation -> IO PingPong }

type PingPong = (Offscreen,Offscreen)

fromLooked :: Looked -> Post
fromLooked lk = Post fromLooked_
  where
    fromLooked_ lighting accumulation = do
      unLooked lk lighting accumulation
      glDisable gl_BLEND
      bindVertexArray (accumulation^.accumVA)
      return (lighting^.lightOff,accumulation^.accumOff)

post :: GPUPostFX a -> a -> Post -> Post
post  gpupfx a prev = Post post_
  where
    post_ lighting accumulation = do
      (sourceOff,targetOff) <- unPost prev lighting accumulation
      usePostFX gpupfx (sourceOff^.offscreenRender) (sourceOff^.offscreenDepthmap) a
      bindFramebuffer (targetOff^.offscreenFB) ReadWrite
      glClear gl_DEPTH_BUFFER_BIT
      glDrawArrays gl_TRIANGLE_STRIP 0 4
      return (targetOff,sourceOff) -- pingpong ! \o/
