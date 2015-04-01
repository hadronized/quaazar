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

import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO )
import Data.Bits ( (.|.) )
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
import Quaazar.Render.Shader ( GPUProgram(..) )
import Quaazar.Render.Texture ( GPUTexture(GPUTexture) )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data Compositor = Compositor {
    _comptVA :: VertexArray -- ^ attributeless vertex array
  }

makeLenses ''Compositor

getCompositor :: (MonadIO m,MonadScoped IO m,MonadLogger m)
              => m Compositor
getCompositor = do
  info RendererLog "generating compositor"
  va <- genAttributelessVertexArray
  return $ Compositor va

newtype CompNode a b = CompNode {
    runCompNode :: Compositor
                -> Buffer -- ^ lighting buffer -- FIXME
                -> a
                -> IO b
  }

compNode :: (MonadIO m,MonadScoped IO m,MonadError Log m)
         => Viewport
         -> GPUProgram a
         -> m (CompNode a (GPUTexture,GPUTexture))
compNode vp prog = do
    Offscreen nodeColor nodeDepth nodeFB <- genOffscreen w h Nearest RGBA32F RGBA
    return . CompNode $ \compt _ a -> do
      -- use the node’s program and send input
      useProgram prog
      sendToProgram prog a
      -- bind the VA
      bindVertexArray (compt^.comptVA)
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
  , "uniform sampler2D source;"
  , "void main() {"
  , " frag = texelFetch(source, ivec2(gl_FragCoord.xy), 0);"
  , "}"
  ]
