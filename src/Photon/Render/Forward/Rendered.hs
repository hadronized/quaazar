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

module Photon.Render.Forward.Rendered where

import Control.Lens
import Data.Monoid ( Monoid(..) )
import Photon.Core.Entity ( Entity )
import Photon.Render.Forward.Lighting
import Photon.Render.Mesh ( GPUMesh(..) )

newtype Rendered = Rendered { unRendered :: Lighting -> IO () }

instance Monoid Rendered where
  mempty = Rendered . const $ return ()
  Rendered f `mappend` Rendered g = Rendered $ \l -> f l >> g l

render :: GPUMesh -> Entity -> Rendered
render gmsh ent = Rendered $ render_
  where
    render_ lighting = renderMesh gmsh (lunis^.lightModelU) ent
      where
        lunis = lighting^.lightUniforms
