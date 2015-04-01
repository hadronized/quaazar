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
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.Compositing
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Looked ( Looked(..) )
import Quaazar.Render.Forward.Viewport ( Viewport(Viewport), setViewport )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer, Target(..)
                                     , bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..)
                                 , bindTextureAt )
import Quaazar.Render.GL.VertexArray ( bindVertexArray )
import Quaazar.Render.PostFX ( GPUPostFX(..) )
import Quaazar.Render.Texture ( GPUTexture(GPUTexture) ) 
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

newtype RenderLayer = RenderLayer {
    unRenderLayer :: Framebuffer -- ^ lighting framebuffer
                  -> Buffer      -- ^ omni light buffer
                  -> IO ()
  }

renderLayer :: Looked -> RenderLayer
renderLayer lk = RenderLayer fromLooked
  where
    fromLooked lightingFB omniBuffer = do
      -- purge accum framebuffer
      unLooked lk lightingFB omniBuffer
      --glDisable gl_BLEND
      --bindVertexArray (accumulation^.accumVA)

renderLayerCompositor :: (MonadIO m,MonadScoped IO m,MonadError Log m)
                      => Viewport
                      -> m (Compositor RenderLayer (GPUTexture,GPUTexture))
renderLayerCompositor vp = do
    Offscreen nodeColor nodeDepth nodeFB <- genOffscreen w h Nearest RGBA32F RGBA
    return . Compositor $ \compt omniBuffer rl -> do
      setViewport vp
      unRenderLayer rl nodeFB omniBuffer
      return (GPUTexture $ bindTextureAt nodeColor,GPUTexture $ bindTextureAt nodeDepth)
  where
    Viewport _ _ w h = vp
