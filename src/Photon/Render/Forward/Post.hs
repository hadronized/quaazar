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

module Photon.Render.Forward.Post where

import Control.Lens
import Graphics.Rendering.OpenGL.Raw
import Photon.Render.Forward.Accumulation
import Photon.Render.Forward.Lighting
import Photon.Render.Forward.Looked ( Looked(..) )
import Photon.Render.Forward.Shadowing
import Photon.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.VertexArray ( bindVertexArray )
import Photon.Render.PostFX ( GPUPostFX(..) )

newtype Post = Post { unPost :: Lighting -> Shadowing -> Accumulation -> IO PingPong }

type PingPong = (Offscreen,Offscreen)

fromLooked :: Looked -> Post
fromLooked lk = Post fromLooked_
  where
    fromLooked_ lighting shadowing accumulation = do
      unLooked lk lighting shadowing accumulation
      return (accumulation^.accumOff,lighting^.lightOff)

post :: GPUPostFX -> Post -> Post
post gpupfx prev = Post post_
  where
    post_ lighting shadowing accumulation = do
      (sourceOff,targetOff) <- unPost prev lighting shadowing accumulation
      usePostFX gpupfx (sourceOff^.offscreenTex)
      bindFramebuffer (targetOff^.offscreenFB) ReadWrite
      glClear gl_DEPTH_BUFFER_BIT
      glDisable gl_BLEND
      bindVertexArray (accumulation^.accumVA)
      glDrawArrays gl_TRIANGLE_STRIP 0 4
      return (targetOff,sourceOff) -- pingpong ! \o/
