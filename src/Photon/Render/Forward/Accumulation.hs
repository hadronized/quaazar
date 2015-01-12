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

module Photon.Render.Forward.Accumulation where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Trans.Either ( EitherT, hoistEither )
import Control.Monad.Trans.Journal ( evalJournalT )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Numeric.Natural ( Natural )
import Photon.Render.GL.Framebuffer ( AttachmentPoint(..), Target(..)
                                    , bindFramebuffer )
import Photon.Render.GL.Offscreen
import Photon.Render.GL.Shader ( (@=), buildProgram
                               , getUniform, useProgram )
import Photon.Render.GL.Texture as Tex ( Format(..), InternalFormat(..) )
import Photon.Render.GL.VertexArray ( VertexArray, genAttributelessVertexArray )
import Photon.Render.Shader ( GPUProgram )
import Photon.Utils.Log

data Accumulation = Accumulation {
    _accumProgram :: GPUProgram
  , _accumOff     :: Offscreen
  , _accumVA      :: VertexArray
  }

makeLenses ''Accumulation

getAccumulation :: Natural -> Natural -> EitherT Log IO Accumulation
getAccumulation w h = do
  program <- evalJournalT $ buildProgram accumVS Nothing accumFS <* sinkLogs
  liftIO . print $ Log InfoLog CoreLog "generating accumulation offscreen"
  off <- liftIO (genOffscreen w h RGB32F RGB (ColorAttachment 0) Depth32F DepthAttachment) >>= hoistEither
  va <- liftIO genAttributelessVertexArray
  liftIO $ do
    useProgram program
    getUniform program "source" >>= (@= (0 :: Int))
  return (Accumulation program off va)

purgeAccumulationFramebuffer :: Accumulation -> IO ()
purgeAccumulationFramebuffer accumulation = do
  bindFramebuffer (accumulation^.accumOff.offscreenFB) ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT

accumVS :: String
accumVS = unlines
  [
    "#version 330 core"
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
    "#version 330 core"
  , "out vec4 frag;"
  , "uniform sampler2D source;"
  , "void main() {"
  , "  frag = texelFetch(source, ivec2(gl_FragCoord.xy), 0);"
  , "}"
  ]
