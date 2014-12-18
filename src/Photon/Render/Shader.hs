{-# LANGUAGE RankNTypes #-}

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

module Photon.Render.Shader where

import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Photon.Render.GL.Shader ( ShaderType(..), Uniform, Uniformable
                               , genProgram, genShader, getUniform )
import qualified Photon.Render.GL.Shader as GL ( useProgram )
import Photon.Utils.Log ( Log, MonadLogger )

data GPUProgram = GPUProgram {
    useProgram :: IO ()
  , programSemantic :: (Uniformable a) => String -> IO (Uniform a)
  }

gpuProgram :: (MonadIO m,MonadLogger m,MonadError Log m) => [(ShaderType,String)] -> m GPUProgram
gpuProgram shaders = do
  program <- mapM (uncurry genShader) shaders >>= genProgram
  return $ GPUProgram (GL.useProgram program) (getUniform program)
