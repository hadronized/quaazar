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

module Quaazar.Render.Forward.Lit where

import Control.Lens
import Data.Bits ( (.|.) )
import Graphics.Rendering.OpenGL.Raw
import Linear
import Quaazar.Core.Color
import Quaazar.Core.Entity
import Quaazar.Core.Light
import Quaazar.Core.Projection ( Projection(..), projectionMatrix )
import Quaazar.Render.Camera ( GPUCamera(..) )
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Shaded ( Shaded(..) )
import Quaazar.Render.Forward.Shadowing
import Quaazar.Render.Forward.Viewport
import Quaazar.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( (@=), unused, useProgram )
import Quaazar.Render.GL.Texture ( bindTextureAt )
import Quaazar.Render.GL.VertexArray ( bindVertexArray )

newtype Lit = Lit { unLit :: Viewport -> Lighting -> Shadowing -> Accumulation -> GPUCamera -> IO () }

lighten :: Ambient ->Shaded -> Lit
lighten (Ambient ligAmbCol ligAmbPow) shd = Lit lighten_
  where
    lighten_ screenViewport lighting shadowing accumulation gpucam = do
        purgeLightingFramebuffer lighting
        useProgram (lighting^.lightProgram)
        lunis^.lightLigAmbCol @= ligAmbCol
        lunis^.lightLigAmbPow @= ligAmbPow
        unShaded shd (lunis^.lightModelU) (lunis^.lightMatDiffAlbU)
          (lunis^.lightMatSpecAlbU) (lunis^.lightMatShnU)
      where
        lunis = lighting^.lightUniforms
