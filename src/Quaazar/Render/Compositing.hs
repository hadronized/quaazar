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
import Control.Applicative ( Applicative(..) )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Data.Bits ( (.|.) )
import Data.Monoid ( Monoid(..) ) 
import Data.Profunctor ( Profunctor(..) ) 
import Data.Semigroup ( Semigroup(..) ) 
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.Forward.Viewport ( Viewport(Viewport), setViewport )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Target(..), bindFramebuffer ) 
import Quaazar.Render.GL.Offscreen ( Offscreen(Offscreen), genOffscreen )
import Quaazar.Render.GL.Texture ( Filter(..), Format(..), InternalFormat(..)
                                 , bindTextureAt )
import Quaazar.Render.GL.VertexArray ( VertexArray, bindVertexArray
                                     , genAttributelessVertexArray )
import Quaazar.Render.Shader
import Quaazar.Render.Texture ( GPUTexture(GPUTexture) )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

import Prelude hiding ( (.), id )

newtype Compositor a b = Compositor {
    runCompositor :: VertexArray -- attribute-less vertex array
                  -> Buffer -- ^ lighting buffer -- FIXME
                  -> a
                  -> IO b
  }

instance Applicative (Compositor a) where
  pure x = Compositor $ \_ _ _ -> pure x
  Compositor f <*> Compositor x = Compositor $ \va b a -> do
    f' <- f va b a
    x' <- x va b a
    pure (f' x')

instance Arrow Compositor where
  arr f = Compositor $ \_ _ a -> pure (f a)
  first (Compositor f) = Compositor $ \va b (x,y) -> do
    x' <- f va b x
    pure (x',y)
  second (Compositor s) = Compositor $ \va b (x,y) -> do
    y' <- s va b y
    pure (x,y')

instance Category Compositor where
  id = Compositor $ \_ _ a -> pure a
  Compositor f . Compositor g = Compositor $ \va b a -> g va b a >>= f va b

instance Functor (Compositor a) where
  fmap f (Compositor g) = Compositor $ \va b a -> fmap f (g va b a)

instance Monad (Compositor a) where
  return = pure
  Compositor x >>= f = Compositor $ \va b a -> do
    x' <- x va b a
    runCompositor (f x') va b a

instance Profunctor Compositor where
  dimap l r (Compositor f) = Compositor $ \va b a -> fmap r $ f va b (l a)

instance (Semigroup b,Monoid b) => Monoid (Compositor a b) where
  mempty = Compositor $ \_ _ _ -> pure mempty
  mappend = (<>)

instance (Semigroup b) => Semigroup (Compositor a b) where
  Compositor x <> Compositor y = Compositor $ \va b a -> do
    x' <- x va b a
    y' <- y va b a
    pure (x' <> y')

-- |This compositor node passes its input to its shader program and outputs both
-- color and depth information as textures.
renderNode :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
           => Viewport
           -> String
           -> (a -> Semantics b)
           -> m (Compositor a (GPUTexture,GPUTexture))
renderNode vp shaderSrc semMapper = do
    Offscreen nodeColor nodeDepth nodeFB <- genOffscreen w h Nearest RGBA32F RGBA
    prog <- gpuProgram copyVS Nothing Nothing shaderSrc semMapper
    return . Compositor $ \va _ a -> do
      -- use the node’s program and send input
      useProgram prog
      sendToProgram prog a
      -- bind the VA
      bindVertexArray va
      -- bind the node’s framebuffer
      bindFramebuffer nodeFB ReadWrite
      glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
      setViewport vp
      glDrawArrays gl_TRIANGLE_STRIP 0 4
      return (GPUTexture $ bindTextureAt nodeColor,GPUTexture $ bindTextureAt nodeDepth)
  where
    Viewport _ _ w h = vp

copyVS :: String
copyVS = unlines
  [
    "#version 430 core"
  , "vec2[4] v = vec2[]("
  , " vec2(-1, 1)"
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
