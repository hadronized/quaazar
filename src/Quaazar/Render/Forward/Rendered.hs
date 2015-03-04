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

newtype Rendered = Rendered {
    unRendered :: Uniform (M44 Float) -- ^ model matrix
               -> IO ()
  }

instance Monoid Rendered where
  mempty = Rendered $ \_ -> return ()
  Rendered f `mappend` Rendered g =
    Rendered $ \m -> f m >> g m

render :: GPUMesh -> IO () -> Entity -> Rendered
render gmsh runMat ent = Rendered $ \modelU -> do
    runMat
    renderMesh gmsh modelU ent
