{-# LANGUAGE ExistentialQuantification #-}

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

module Quaazar.Scene.World (
    -- * Sub-worlds
    SubWorld
  , subWorld
    -- * Worlds
  , World(..)
  ) where

import Quaazar.Lighting.Light ( Ambient, Omni )
import Quaazar.Render.GL.Shader ( Program' )
import Quaazar.Scene.Hierarchy ( Instance )
import Quaazar.Scene.Model ( Model )

data SubWorld = forall a. SubWorld (Program' a) [Instance (Model a)]

subWorld :: Program' a -> [Instance (Model a)] -> SubWorld
subWorld = SubWorld

data World = World Ambient [Omni] [SubWorld]
