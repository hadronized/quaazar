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

import Control.Lens ( makeLenses )
import Control.Monad.Error.Class ( MonadError )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Traversable ( traverse )
import Data.Vector ( Vector, (!?), fromList )
import Graphics.Rendering.OpenGL.Raw
import Photon.Render.GL.Shader ( Program, ShaderType(..), Uniform, Uniformable
                               , genProgram, genShader, getUniformLocation
                               , uniform )
import qualified Photon.Render.GL.Shader as GL ( useProgram )
import Photon.Utils.Log ( MonadLogger )

data GPUProgram = GPUProgram {
    _gpuShaderProgram   :: Program
  , _gpuShaderSemantics :: Vector GLint
  }

makeLenses ''GPUProgram

gpuProgram :: (MonadIO m,MonadLogger m,MonadError String m) => [(ShaderType,String)] -> [String] -> m GPUProgram
gpuProgram shaders semantics = do
  program <- mapM (uncurry genShader) shaders >>= genProgram
  semantics' <- liftIO . fmap fromList $ traverse (getUniformLocation program) semantics
  return (GPUProgram program semantics')

programSemantic :: (Uniformable a) => GPUProgram -> Int -> Maybe (Uniform a)
programSemantic (GPUProgram _ semantics) sem = fmap uniform (semantics !? sem)

useProgram :: (MonadIO m) => GPUProgram -> m ()
useProgram (GPUProgram p _) = liftIO (GL.useProgram p)
