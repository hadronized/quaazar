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

module Quaazar.Render.Forward.Rendered where

import Data.Monoid ( Monoid(..) )
import Linear ( M44 )
import Quaazar.Core.Entity ( Entity )
import Quaazar.Render.GL.Shader ( Uniform )
import Quaazar.Render.Mesh ( GPUMesh(..) )

newtype Rendered = Rendered { unRendered :: Uniform (M44 Float) -> IO () }

instance Monoid Rendered where
  mempty = Rendered . const $ return ()
  Rendered f `mappend` Rendered g = Rendered $ \l -> f l >> g l

render :: GPUMesh -> Entity -> Rendered
render gmsh ent = Rendered $ render_
  where
    render_ modelU = renderMesh gmsh modelU ent
