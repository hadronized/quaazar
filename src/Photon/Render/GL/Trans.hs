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

module Photon.Render.GL.Trans (
    -- *
  ) where

import Control.Applicative
import Control.Monad.Trans.State ( StateT, runStateT )
import Data.Foldable
import Data.Traversable
import Data.Vector ( Vector )
import Photon.Core.Effect
import Photon.Core.Light ( Light )

newtype OpenGLT m a = OpenGLT { runOpenGLT :: StateT OpenGLSt m a } deriving (Applicative,Foldable,Functor,Monad)

instance (Foldable m) => Foldable (OpenGLT m) where
  foldMap
instance (Traversable m) => Traversable (OpenGLT m) where
  traverse f (OpenGLT st) = fmap OpenGLT (traverse st)

data OpenGLSt = OpenGLSt {
    glStLights :: Vector (Managed Light)
  } deriving (Eq,Show)

