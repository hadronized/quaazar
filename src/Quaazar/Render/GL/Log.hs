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

module Quaazar.Render.GL.Log (
  -- * OpenGL logs
  gllog
  ) where

import Quaazar.Utils.Log ( LogCommitter(BackendLog) )

gllog :: LogCommitter
gllog = BackendLog "gl"