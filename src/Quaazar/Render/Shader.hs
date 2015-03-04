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

module Quaazar.Render.Shader (
    GPUProgram(useProgram)
  , gpuProgram
  ) where

import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Quaazar.Render.GL.Shader ( buildProgram )
import qualified Quaazar.Render.GL.Shader as GL ( useProgram )
import Quaazar.Utils.Log
import Quaazar.Utils.Scoped

newtype GPUProgram a = GPUProgram { useProgram :: a -> IO () }

gpuProgram :: (MonadScoped IO m,MonadIO m,MonadLogger m,MonadError Log m)
           => String
           -> Maybe String
           -> String
           -> (a -> IO ())
           -> m (GPUProgram a)
gpuProgram vs gs fs update = do
    program <- buildProgram vs gs fs
    return . GPUProgram $ \a -> do
      GL.useProgram program
      update a
