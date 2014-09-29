-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Renderer interface. A 'Renderer' is an object that exposes a few useful
-- functions:
--   - 'render': perform a render;
--   - 'display': display a render;
--   - 'postfx': apply post-process effects;
--   - 'screenshot': write a render on the disk.
--
-- Furthermore, this module exports the notion of 'PostFX'.
----------------------------------------------------------------------------

module Photon.Core.Renderer (
    -- * Renderer
    Renderer(..)
  ) where

import Photon.Core.Scene ( IndexPath, Scene )
import Photon.Core.PostFX ( FrameShader, PostFX )

-- |Renderers are plain data that host rendering functions. They’re generated
-- for a specific `Scene a`.
data Renderer frame = Renderer {
    -- |Render a scene into a frame. This frame is just a rendered version
    -- of the scene, nothing more.
    render             :: Scene IndexPath -> frame
    -- |Compile a frame shader into a post-process.
  , compileFrameShader :: FrameShader -> PostFX frame
    -- |Apply a list of post-processes on a frame, and return the new frame.
  , postfx             :: [PostFX frame] -> frame -> frame
    -- |Display a rendered scene (frame). That function should render the
    -- frame onto the screen / dedicated area for rendering.
  , display            :: frame -> IO ()
    -- |Write a frame in a file as a PNG image.
  , screenshot         :: FilePath -> frame -> IO ()
  }
