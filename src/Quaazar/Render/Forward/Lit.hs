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
import Data.Monoid ( Monoid(..) )
import Quaazar.Core.Entity
import Quaazar.Core.Light
import Quaazar.Render.Forward.Accumulation
import Quaazar.Render.Forward.Lighting
import Quaazar.Render.Forward.Rendered ( Rendered(..) )
import Quaazar.Render.Forward.Shadowing
import Quaazar.Render.GL.Shader ( (@=) )

newtype Lit = Lit { unLit :: Lighting -> Shadowing -> Accumulation -> IO () }

instance Monoid Lit where
  mempty = Lit $ \_ _ _ -> return ()
  Lit f `mappend` Lit g = Lit $ \l s a -> f l s a >> g l s a

lighten :: Ambient -> [(Omni,Entity)] -> Rendered -> Lit
lighten (Ambient ligAmbCol ligAmbPow) omnis shd = Lit lighten_
  where
    lighten_ lighting _ _ = do
        purgeLightingFramebuffer lighting
        lunis^.lightLigAmbCol @= ligAmbCol
        lunis^.lightLigAmbPow @= ligAmbPow
        pushOmnis omnis lighting
        unRendered shd (lunis^.lightModelU)
      where
        lunis = lighting^.lightUniforms
