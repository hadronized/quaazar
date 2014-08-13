module Photon.Core.Renderer (
    -- * Renderer
    Renderer(..)
    -- * Post-process
  , PostFX(..)
  ) where

import Photon.Core.Entity ( Entities )
import Photon.Core.Scene ( IndexPath )

-- |Renderers are plain data that host rendering functions. They’re generated
-- for a specific `Scene a`.
data Renderer frame = Renderer {
    -- |Render scene entities into a frame. This frame is just a rendered
    -- version of the scene, nothing more.
    render     :: Entities IndexPath -> frame
    -- |Apply a list of post-processes on a frame, and return the new frame.
  , postfx     :: [PostFX frame] -> frame -> frame
    -- |Display a rendered scene (frame). That function should render the
    -- frame onto the screen / dedicated area for rendering.
  , display    :: frame -> IO ()
    -- |Write a frame in a file as a PNG image.
  , screenshot :: FilePath -> frame -> IO ()
  }

-- |A post-process effect is an endormophism between two frames. It’s used to
-- modify a frame.
newtype PostFX frame = PostFX { runPostFX :: frame -> frame }
