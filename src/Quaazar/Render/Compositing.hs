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

module Quaazar.Render.Compositing where

import Control.Arrow ( Arrow(..) )
import Control.Category ( Category(..) ) 
import Control.Lens
import Control.Monad ( (>=>) )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Data.Bits ( (.|.) )
import Data.Semigroup ( Semigroup(..) ) 
import Graphics.Rendering.OpenGL.Raw
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Target(..), bindFramebuffer ) 
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Primitive
import Quaazar.Render.GL.Shader
import Quaazar.Render.GL.Texture
import Quaazar.Render.GL.VertexArray
import Quaazar.Render.Light ( ShadowConf )
import Quaazar.Render.Lighting ( Shadows )
import Quaazar.Render.Semantics
import Quaazar.Render.RenderLayer
import Quaazar.Render.Viewport
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

import Prelude hiding ( (.), id, last, maximum )

newtype Compositor a b = Compositor {
    runCompositor :: VertexArray -- attribute-less vertex array
                  -> Buffer -- lighting buffer -- FIXME
                  -> Maybe (ShadowConf,Shadows) -- FIXME
                  -> a
                  -> IO b
  }

instance Applicative (Compositor a) where
  pure = arr . const
  Compositor f <*> Compositor x = Compositor $ \va b s a -> do
    f' <- f va b s a
    x' <- x va b s a
    pure (f' x')

instance Arrow Compositor where
  arr f = Compositor $ \_ _ _ a -> pure (f a)
  first (Compositor f) = Compositor $ \va b s (x,y) -> do
    x' <- f va b s x
    pure (x',y)
  second (Compositor s) = Compositor $ \va b shdw (x,y) -> do
    y' <- s va b shdw y
    pure (x,y')

instance Category Compositor where
  id = arr id
  Compositor f . Compositor g = Compositor $ \va b s -> g va b s >=> f va b s

instance Functor (Compositor a) where
  fmap f (Compositor g) = Compositor $ \va b s a -> fmap f (g va b s a)

instance Monad (Compositor a) where
  return = pure
  Compositor x >>= f = Compositor $ \va b s a -> do
    x' <- x va b s a
    runCompositor (f x') va b s a

instance Profunctor Compositor where
  dimap l r (Compositor f) = Compositor $ \va b s a -> do
    fmap r $ f va b s (l a)

instance (Semigroup b,Monoid b) => Monoid (Compositor a b) where
  mempty = Compositor $ \_ _ _ _ -> pure mempty
  mappend = (<>)

instance (Semigroup b) => Semigroup (Compositor a b) where
  Compositor x <> Compositor y = Compositor $ \va b s a -> do
    x' <- x va b s a
    y' <- y va b s a
    pure (x' <> y')

-- TODO: optimize by removing the depthmap
-- |This compositor node passes its input to its shader program and outputs both
-- color and depth information as textures.
--
-- **Note**: you’re advised to use the 'buildPostProcessProgram' function to
-- generate the node program.
postProcessNode :: (MonadIO m,MonadScoped IO m,MonadError Log m)
                => Viewport 
                -> Program 
                -> (a -> ShaderSemantics ())
                -> m (Compositor a Texture2D)
postProcessNode vp prog semantics = do
    Offscreen colormap _ fb <- genOffscreen w h Linear RGBA32F
    pure . Compositor $ \va _ _ a -> do
      bindFramebuffer fb ReadWrite
      glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
      -- use the node’s program and send input
      useProgram prog
      _ <- runShaderSemantics $ semantics a
      -- bind the VA & the compositing framebuffer
      bindVertexArray va
      -- render the shit
      setViewport vp
      drawArrays STriangle 0 4
      pure colormap
  where
    Viewport _ _ w h = vp

-- |Help users to build a post-process shader program.
buildPostProcessProgram :: (MonadIO m,MonadScoped IO m,MonadError Log m,MonadLogger m)
                        => String
                        -> (a -> ShaderSemantics ())
                        -> m (Program,a -> ShaderSemantics ())
buildPostProcessProgram fs semantics =
  (,semantics) <$> buildProgram copyVS Nothing Nothing fs

-- |This compositor node absorbs a 'RenderLayer'.
renderNode :: (MonadIO m,MonadScoped IO m,MonadError Log m)
           => Viewport 
           -> m (Compositor RenderLayer (Texture2D,Texture2D))
renderNode vp = do
    Offscreen colormap depthmap fb <- genOffscreen w h Linear RGBA32F
    pure . Compositor $ \_ omniBuffer shadowsConf rl -> do
      setViewport vp
      unRenderLayer rl fb omniBuffer shadowsConf
      pure (colormap,depthmap)
  where
    Viewport _ _ w h = vp

colormapUniform :: Uniform (Texture2D,Unit)
colormapUniform = toUniform ColormapSem

depthmapUniform :: Uniform (Texture2D,Unit)
depthmapUniform = toUniform DepthmapSem

copyVS :: String
copyVS = unlines
  [
    "#version 430 core"

  , "vec2[4] v = vec2[]("
  , "   vec2(-1, 1)"
  , " , vec2( 1, 1)"
  , " , vec2(-1, -1)"
  , " , vec2( 1, -1)"
  , " );"

  , "void main() {"
  , " gl_Position = vec4(v[gl_VertexID], 0., 1.);"
  , "}"
  ]

copyFS :: String
copyFS = unlines
  [
    "#version 430 core"
  , "out vec4 frag;"

  , declUniform ColormapSem "sampler2D source"

  , "void main() {"
  , " frag = texelFetch(source, ivec2(gl_FragCoord.xy), 0);"
  , "}"
  ]
