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
import Data.Monoid ( Monoid(..) )
import Quaazar.Core.Light
import Quaazar.Core.Transform
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Rendered ( Rendered(..) )
import Quaazar.Render.Forward.Shadowing
import Quaazar.Render.GL.Shader ( (@=) )

newtype Lit mat = Lit {
    unLit :: Lighting
          -> Shadowing
          -> Accumulation
          -> (mat -> IO ())
          -> IO ()
  }

lighten :: Ambient -> [(Omni,Transform)] -> Rendered mat -> Lit mat
lighten (Ambient ligAmbCol ligAmbPow) omnis shd = Lit lighten_
  where
    lighten_ lighting _ _ sinkMat = do
        purgeLightingFramebuffer lighting
        lunis^.lightLigAmbCol @= ligAmbCol
        lunis^.lightLigAmbPow @= ligAmbPow
        pushOmnis omnis lighting
        unRendered shd (lunis^.lightModelU) sinkMat
      where
        lunis = lighting^.lightUniforms
