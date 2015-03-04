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
import Data.Monoid ( Monoid(..) )
import Graphics.Rendering.OpenGL.Raw
import Linear
import Quaazar.Core.Color
import Quaazar.Core.Entity
import Quaazar.Core.Light
import Quaazar.Core.Projection ( Projection(..), projectionMatrix )
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Rendered ( Rendered(..) )
import Quaazar.Render.Forward.Shadowing
import Quaazar.Render.Forward.Viewport
import Quaazar.Render.GL.Framebuffer ( Target(..), bindFramebuffer )
import Quaazar.Render.GL.Offscreen
import Quaazar.Render.GL.Shader ( (@=), unused, useProgram )
import Quaazar.Render.GL.Texture ( bindTextureAt )
import Quaazar.Render.GL.VertexArray ( bindVertexArray )

newtype Lit = Lit { unLit :: Lighting -> Shadowing -> Accumulation -> IO () }

instance Monoid Lit where
  mempty = Lit $ \_ _ _ -> return ()
  Lit f `mappend` Lit g = Lit $ \l s a -> f l s a >> g l s a

lighten :: Ambient -> [(Omni,Entity)] -> Rendered -> Lit
lighten (Ambient ligAmbCol ligAmbPow) omnis shd = Lit lighten_
  where
    lighten_ lighting shadowing accumulation = do
        purgeLightingFramebuffer lighting
        lunis^.lightLigAmbCol @= ligAmbCol
        lunis^.lightLigAmbPow @= ligAmbPow
        pushOmnis omnis lighting
        unRendered shd (lunis^.lightModelU)
      where
        lunis = lighting^.lightUniforms
