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

module Photon.Render.OpenGL.Forward.Frame where

import Photon.Render.OpenGL.Texture ( Texture )

-- |An /OpenGL/ frame is an OpenGL-side texture wrapped in 'IO'.
type Frame = IO Texture
