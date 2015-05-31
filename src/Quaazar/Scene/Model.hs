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

module Quaazar.Scene.Model (
    -- * Model
    Model(..)
  , unModel
  ) where

import Quaazar.Render.Mesh ( GPUMesh )

data Model a = Model !GPUMesh !a

instance Functor Model where
  fmap f (Model a b) = Model a (f b)

unModel :: Model a -> (GPUMesh,a)
unModel (Model gmesh mat) = (gmesh,mat)
