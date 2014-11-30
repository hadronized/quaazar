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

module Photon.Render.GL.Forward.Log where

import Photon.Utils.Log ( LogCommitter(BackendLog) )

gllog :: LogCommitter
gllog = BackendLog "gl-fwd"
