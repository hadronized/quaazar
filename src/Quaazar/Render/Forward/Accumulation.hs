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

module Quaazar.Render.Forward.Accumulation where

import Control.Applicative
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Bits ( (.|.) )
import Data.Int ( Int32 )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Quaazar.Render.GL.Framebuffer (Target(..), bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( Program, (@=), buildProgram, getUniform
                               , useProgram )
import Quaazar.Render.GL.Texture as Tex ( Filter(..)
                                       , Format(..), InternalFormat(..) )
import Quaazar.Render.GL.VertexArray ( VertexArray, genAttributelessVertexArray )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data Accumulation = Accumulation {
    _accumProgram :: Program
  , _accumOff     :: Offscreen
  , _accumOff2    :: Offscreen
  , _accumVA      :: VertexArray
  }

makeLenses ''Accumulation

getAccumulation :: (Applicative m,MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
                => Natural
                -> Natural
                -> m Accumulation
getAccumulation w h = do
  program <- buildProgram accumVS Nothing accumFS <* sinkLogs
  info CoreLog "generating accumulation offscreen"
  off <- genOffscreen w h Nearest RGB32F RGB -- TODO: color offscreen
  off2 <- genOffscreen w h Nearest RGB32F RGB -- TODO: color offscreen
  va <- genAttributelessVertexArray
  liftIO $ do
    useProgram program
    getUniform program "source" >>= (@= (0 :: Int32))
  return (Accumulation program off off2 va)

purgeAccumulationFramebuffer :: (MonadIO m) => Accumulation -> m ()
purgeAccumulationFramebuffer accumulation = liftIO $ do
  bindFramebuffer (accumulation^.accumOff.offscreenFB) ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT

purgeAccumulationFramebuffer2 :: (MonadIO m) => Accumulation -> m ()
purgeAccumulationFramebuffer2 accumulation = liftIO $ do
  bindFramebuffer (accumulation^.accumOff2.offscreenFB) ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT

accumVS :: String
accumVS = unlines
  [
    "#version 430 core"

  , "vec2[4] v = vec2[]("
  , "    vec2(-1,  1)"
  , "  , vec2( 1,  1)"
  , "  , vec2(-1, -1)"
  , "  , vec2( 1, -1)"
  , "  );"

  , "void main() {"
  , "  gl_Position = vec4(v[gl_VertexID], 0., 1.);"
  , "}"
  ]

accumFS :: String
accumFS = unlines
  [
    "#version 430 core"

  , "out vec4 frag;"

  , "uniform sampler2D source;"

  , "void main() {"
  , "  frag = texelFetch(source, ivec2(gl_FragCoord.xy), 0);"
  , "}"
  ]