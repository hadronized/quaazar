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
import Photon.Core.Entity ( Entity )
import Photon.Render.Forward.Lighting
import Photon.Render.Mesh ( GPUMesh(..) )

newtype Rendered = Rendered { unRendered :: Lighting -> IO () }

render :: GPUMesh -> Entity -> Rendered
render gmsh ent = Rendered $ render_
  where
    render_ lighting = renderMesh gmsh (lunis^.lightModelU) ent
      where
        lunis = lighting^.lightUniforms
