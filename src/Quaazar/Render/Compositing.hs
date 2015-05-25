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
import Control.Monad.Trans ( MonadIO, lift )
import Control.Monad.Trans.State ( StateT, get, modify )
import Data.Bits ( (.|.) )
import Data.Semigroup ( Semigroup(..) ) 
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.Viewport ( Viewport, setViewport )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer, Target(..)
                                     , bindFramebuffer ) 
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader
import Quaazar.Render.GL.Texture
import Quaazar.Render.GL.VertexArray ( VertexArray, bindVertexArray )
import Quaazar.Render.Light ( ShadowConf )
import Quaazar.Render.Lighting ( Shadows )
import Quaazar.Render.Semantics
import Quaazar.Render.RenderLayer
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

import Prelude hiding ( (.), id, last, maximum )

newtype Compositor a b = Compositor {
    runCompositor :: OffscreenArray -- compositing offscreen
                  -> VertexArray -- attribute-less vertex array
                  -> Buffer -- lighting buffer -- FIXME
                  -> Maybe (ShadowConf,Shadows) -- FIXME
                  -> a
                  -> StateT Natural IO b
  }

instance Applicative (Compositor a) where
  pure = arr . const
  Compositor f <*> Compositor x = Compositor $ \fb va b s a -> do
    f' <- f fb va b s a
    x' <- x fb va b s a
    pure (f' x')

instance Arrow Compositor where
  arr f = Compositor $ \_ _ _ _ a -> pure (f a)
  first (Compositor f) = Compositor $ \fb va b s (x,y) -> do
    x' <- f fb va b s x
    pure (x',y)
  second (Compositor s) = Compositor $ \fb va b shdw (x,y) -> do
    y' <- s fb va b shdw y
    pure (x,y')

instance Category Compositor where
  id = arr id
  Compositor f . Compositor g = Compositor $ \fb va b s -> g fb va b s >=> f fb va b s

instance Functor (Compositor a) where
  fmap f (Compositor g) = Compositor $ \fb va b s a -> fmap f (g fb va b s a)

instance Monad (Compositor a) where
  return = pure
  Compositor x >>= f = Compositor $ \fb va b s a -> do
    x' <- x fb va b s a
    runCompositor (f x') fb va b s a

instance Profunctor Compositor where
  dimap l r (Compositor f) = Compositor $ \fb va b s a -> do
    fmap r $ f fb va b s (l a)

instance (Semigroup b,Monoid b) => Monoid (Compositor a b) where
  mempty = Compositor $ \_ _ _ _ _ -> pure mempty
  mappend = (<>)

instance (Semigroup b) => Semigroup (Compositor a b) where
  Compositor x <> Compositor y = Compositor $ \fb va b s a -> do
    x' <- x fb va b s a
    y' <- y fb va b s a
    pure (x' <> y')

-- |This compositor node passes its input to its shader program and outputs both
-- color and depth information as textures.
newNode :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
        => Viewport
        -> Program' a 
        -> m (Compositor a Layer)
newNode vp (prog,semantics) = do
  pure . Compositor $ \compositing va _ _ a -> do
    -- get the next layer
    layer <- fmap Layer get
    modify succ
    -- use it
    lift $ do
      -- use the node’s program and send input
      useProgram prog
      _ <- runShaderSemantics $ semantics a
      layerUniform @= layer
      compositingColormapsUniform @= (compositing^.offscreenArrayColormaps,Unit 0)
      compositingDepthmapsUniform @= (compositing^.offscreenArrayDepthmaps,Unit 1)
      -- bind the VA & the compositing framebuffer
      bindVertexArray va
      bindFramebuffer (compositing^.offscreenArrayFB) ReadWrite
      -- render the shit
      glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
      setViewport vp
      glDrawArrays gl_TRIANGLE_STRIP 0 4
      pure layer

-- |This compositor node absorbs a 'RenderLayer'.
newRLNode :: (MonadIO m,MonadScoped IO m,MonadError Log m)
          => Viewport
          -> m (Compositor RenderLayer Layer)
newRLNode vp = do
    pure . Compositor $ \compositing _ omniBuffer shadowsConf rl -> do
      -- get next layer 
      layer <- fmap Layer get
      modify succ
      -- use it
      lift $ do
        setViewport vp
        unRenderLayer rl (compositing^.offscreenArrayFB) omniBuffer shadowsConf layer
        pure layer

compositingColormapsUniform :: Uniform (Texture2DArray,Unit)
compositingColormapsUniform = toUniform CompositingColormapsSem

compositingDepthmapsUniform :: Uniform (Texture2DArray,Unit)
compositingDepthmapsUniform = toUniform CompositingDepthmapsSem

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
  , "layout (location = 0) uniform sampler2D source;"
  , "void main() {"
  , " frag = texelFetch(source, ivec2(gl_FragCoord.xy), 0);"
  , "}"
  ]
