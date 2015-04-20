-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- OpenGL log committer.
----------------------------------------------------------------------------

module Quaazar.Render.GL.Log (
  -- * OpenGL logs
  gllog
  ) where

import Quaazar.Utils.Log ( LogCommitter(BackendLog) )

-- |OpenGL 'LogCommitter'.
gllog :: LogCommitter
gllog = BackendLog "gl"
