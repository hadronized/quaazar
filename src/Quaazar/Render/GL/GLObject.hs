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

class GLObject a where
  -- |Generate a new OpenGL object and put in it in scope.
  genObject :: (MonadScoped IO m) => m a
  genObject = fmap head (genObjects 1)
  -- |Generate a few OpenGL objects and put them in scope.
  genObjects :: (MonadScoped IO m) => Int -> m [a]
  genObjects n = replicateM n genObject

-- |A lot of OpenGL objects are generated via functions that take the number of
-- objects to allocate and a pointer to the array to store the objects within
-- (and the same for deallocating).
--
-- This function captures that pattern for generice OpenGL objects.
genericGenObjects :: (MonadScoped IO m)
                  => Int -- ^ number of objects to allocate
                  -> (GLsizei -> Ptr GLuint -> IO ()) -- ^ allocator
                  -> (GLsizei -> Ptr GLuint -> IO ()) -- ^ deallocator
                  -> (GLuint -> a) -- ^ Haskell wrapper
                  -> m [a]
genericGenObjects n alloc dealloc wrapper = do
    p <- liftBase $ do
      p <- malloc
      alloc (fromIntegral n) p
      return p
    scoped $ dealloc (fromIntegral n) p >> free p
    liftBase $ fmap (map wrapper) (peekArray n p)
