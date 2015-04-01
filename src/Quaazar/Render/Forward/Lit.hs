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

module Quaazar.Render.Forward.Lit where

import Control.Lens
import Data.Bits ( (.|.) )
import Data.Monoid ( Monoid(..) )
import Graphics.Rendering.OpenGL.Raw
import Quaazar.Core.Light
import Quaazar.Core.Transform
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Rendered ( Rendered(..) )
import Quaazar.Render.GL.Buffer ( Buffer )
import Quaazar.Render.GL.Framebuffer ( Framebuffer, Target(..)
                                     , bindFramebuffer )
import Quaazar.Render.GL.Shader ( (@=) )

newtype Lit mat = Lit {
    unLit :: Framebuffer    -- ^ lighting framebuffer
          -> Buffer         -- ^ omni light buffer
          -> (mat -> IO ()) -- ^ material sink
          -> IO ()
  }

lighten :: Ambient -> [(Omni,Transform)] -> Rendered mat -> Lit mat
lighten (Ambient ligAmbCol ligAmbPow) omnis shd = Lit lighten_
  where
    lighten_ lightingFB omniBuffer sinkMat = do
      purgeLightingFramebuffer lightingFB
      ligAmbColUniform  @= ligAmbCol
      ligAmbPowUniform @= ligAmbPow
      pushOmnis omnis omniBuffer
      unRendered shd modelUniform sinkMat

purgeLightingFramebuffer :: Framebuffer -> IO ()
purgeLightingFramebuffer fb = do
  bindFramebuffer fb ReadWrite
  glClearColor 0 0 0 0
  glClear $ gl_DEPTH_BUFFER_BIT .|. gl_COLOR_BUFFER_BIT
