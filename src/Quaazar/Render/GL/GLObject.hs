-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- OpenGL has a notion of “objects”. OpenGL objects tend to follow the
-- same life-cycle pattern:
--
--   - they’re often created with a 'glGen*' or 'glCreate*' functions that
--     write the OpenGL objects identifiers to a provided pointer
--   - they’re often destroyed with a 'glDelete*' function that takes the
--     previous pointer and frees up the memory
--
-- That pattern is captured with the 'GLObject' typeclass. It uses the
-- 'MonadScoped' idea from "Quaazar.Utils.Scoped".
----------------------------------------------------------------------------

module Quaazar.Render.GL.GLObject (
    -- * OpenGL object
    GLObject(..)
  , genericGenObjects
    -- * Re-exported
  , module Quaazar.Utils.Scoped
  , GLuint
  ) where

import Control.Monad ( replicateM )
import Foreign.Marshal ( malloc, free )
import Foreign.Marshal.Array ( peekArray )
import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw
import Quaazar.Utils.Scoped

-- |OpenGL object allocation/deallocation pattern.
class GLObject a where
  -- |Generate a new OpenGL object.
  genObject :: (MonadScoped IO m) => m a
  genObject = fmap head (genObjects 1)
  -- |Generate a few OpenGL objects..
  genObjects :: (MonadScoped IO m) => Int -> m [a]
  genObjects n = replicateM n genObject

-- |A lot of OpenGL objects are generated via functions that take the number of
-- objects to allocate and a pointer to the array to store the objects within
-- (and the same for deallocating).
--
-- This function captures that pattern for generic OpenGL objects.
genericGenObjects :: (MonadScoped IO m)
                  => Int -- ^ number of objects to allocate
                  -> (GLsizei -> Ptr GLuint -> IO ()) -- ^ allocator
                  -> (GLsizei -> Ptr GLuint -> IO ()) -- ^ deallocator
                  -> (GLuint -> a) -- ^ wrapper
                  -> m [a]
genericGenObjects n alloc dealloc wrapper = do
    p <- liftBase $ do
      p <- malloc
      alloc (fromIntegral n) p
      return p
    scoped $ dealloc (fromIntegral n) p >> free p
    liftBase $ fmap (map wrapper) (peekArray n p)
