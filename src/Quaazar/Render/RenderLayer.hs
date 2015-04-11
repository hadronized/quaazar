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

module Quaazar.Render.RenderLayer where

import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.Compositing
import Quaazar.Render.Lighting
import Quaazar.Render.Looked ( Looked(..) )
import Quaazar.Render.Viewport ( Viewport(Viewport), setViewport )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer, Target(..)
                                     , bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..)
                                 , bindTextureAt )
import Quaazar.Render.GL.VertexArray ( bindVertexArray )
import Quaazar.Render.Light ( ShadowConf )
import Quaazar.Render.Lighting ( Shadows )
import Quaazar.Render.Texture ( GPUTexture(GPUTexture) ) 
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

newtype RenderLayer = RenderLayer {
    unRenderLayer :: Framebuffer                -- ^ lighting framebuffer
                  -> Buffer                     -- ^ omni light buffer
                  -> Maybe (ShadowConf,Shadows) -- ^ shadows configuration
                  -> IO ()
  }

renderLayer :: Looked -> RenderLayer
renderLayer lk = RenderLayer $ unLooked lk

renderLayerCompositor :: (MonadIO m,MonadScoped IO m,MonadError Log m)
                      => Viewport
                      -> m (Compositor RenderLayer (GPUTexture,GPUTexture))
renderLayerCompositor vp = do
    Offscreen nodeColor nodeDepth nodeFB <- genOffscreen w h Nearest RGBA32F RGBA
    return . Compositor $ \_ omniBuffer shadowsConf rl -> do
      setViewport vp
      unRenderLayer rl nodeFB omniBuffer shadowsConf
      return (GPUTexture $ bindTextureAt nodeColor,GPUTexture $ bindTextureAt nodeDepth)
  where
    Viewport _ _ w h = vp
