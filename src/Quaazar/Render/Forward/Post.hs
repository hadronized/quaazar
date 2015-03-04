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

module Quaazar.Render.Forward.Post where

import Control.Lens
import Graphics.Rendering.OpenGL.Raw
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Looked ( Looked(..) )
import Quaazar.Render.Forward.Shadowing
import Quaazar.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.VertexArray ( bindVertexArray )
import Quaazar.Render.PostFX ( GPUPostFX(..) )

newtype Post = Post { unPost :: Lighting -> Shadowing -> Accumulation -> IO PingPong }

type PingPong = (Offscreen,Offscreen)

fromLooked :: Looked -> Post
fromLooked lk = Post fromLooked_
  where
    fromLooked_ lighting shadowing accumulation = do
      unLooked lk lighting shadowing accumulation
      glDisable gl_BLEND
      bindVertexArray (accumulation^.accumVA)
      return (lighting^.lightOff,accumulation^.accumOff)

post :: GPUPostFX a -> a -> Post -> Post
post  gpupfx a prev = Post post_
  where
    post_ lighting shadowing accumulation = do
      (sourceOff,targetOff) <- unPost prev lighting shadowing accumulation
      usePostFX gpupfx (sourceOff^.offscreenRender) a
      bindFramebuffer (targetOff^.offscreenFB) ReadWrite
      glClear gl_DEPTH_BUFFER_BIT
      glDrawArrays gl_TRIANGLE_STRIP 0 4
      return (targetOff,sourceOff) -- pingpong ! \o/
