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

module Photon.Render.GL.GLObject (
    -- * OpenGL object
    GLObject(..)
  , withGLObject
  ) where

import Control.Monad ( (>=>) )
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import Foreign.Storable ( peek )
import Graphics.Rendering.OpenGL.Raw ( GLuint )

newtype GLObject = GLObject { unGLObject :: ForeignPtr GLuint } deriving (Eq,Show)

withGLObject :: GLObject -> (GLuint -> IO a) -> IO a
withGLObject o f = withForeignPtr (unGLObject o) (peek >=> f)
