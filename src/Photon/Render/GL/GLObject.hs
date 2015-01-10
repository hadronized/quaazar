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
    -- * Re-exported
  , GLuint
  ) where

import Control.Monad ( replicateM )
import Graphics.Rendering.OpenGL.Raw ( GLuint )

class GLObject a where
  -- |
  genObject :: IO a
  genObject = fmap head (genObjects 1)
  -- |
  genObjects :: Int -> IO [a]
  genObjects n = replicateM n genObject
  -- |
  deleteObject :: a -> IO ()
  deleteObject a = deleteObjects [a]
  -- |
  deleteObjects :: [a] -> IO ()
  deleteObjects = mapM_ deleteObject
