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

module Quaazar.Render.Renderer where

import Control.Applicative
import Control.Lens
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Graphics.UI.GLFW ( Window, swapBuffers )
import Numeric.Natural ( Natural )
import Quaazar.Render.Compositing ( Compositor(..), copyVS, copyFS )
import Quaazar.Render.GL.Framebuffer ( Target(ReadWrite), unbindFramebuffer )
import Quaazar.Render.GL.Shader ( Program, buildProgram, useProgram )
import Quaazar.Render.GL.VertexArray ( VertexArray, bindVertexArray
                                     , genAttributelessVertexArray )
import Quaazar.Render.Light
import Quaazar.Render.Lighting ( Lighting, getLighting, lightOmniBuffer
                               , shadows )
import Quaazar.Render.Texture ( GPUTexture(..) )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

data Renderer = Renderer {
    _frCopyProgram :: Program
  , _frLighting    :: Lighting
  , _frVA          :: VertexArray
  , _frWindow      :: Window
  }

makeLenses ''Renderer

getRenderer :: (Applicative m,MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
            => Natural
            -> Natural
            -> Natural
            -> Maybe ShadowConf
            -> Window
            -> m Renderer
getRenderer w h lightMaxNb shadowConf window =
  Renderer
    <$> getCopyProgram
    <*> getLighting w h lightMaxNb shadowConf
    <*> genAttributelessVertexArray
    <*> pure window

getCopyProgram :: (MonadIO m,MonadScoped IO m,MonadLogger m,MonadError Log m)
               => m Program
getCopyProgram = buildProgram copyVS Nothing Nothing copyFS

display :: (MonadIO m) => Renderer -> a -> Compositor a GPUTexture -> m ()
display rdr a compt = liftIO $ do
  source <- runCompositor compt (rdr^.frVA) (rdr^.frLighting.lightOmniBuffer)
    (rdr^.frLighting.shadows) a
  useProgram (rdr^.frCopyProgram)
  unbindFramebuffer ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  bindVertexArray (rdr^.frVA)
  bindTextureAt source 0
  glDrawArrays gl_TRIANGLE_STRIP 0 4
  liftIO $ swapBuffers (rdr^.frWindow)
