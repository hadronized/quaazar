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

module Quaazar.Scene.Retina (
    -- * Retina
    Retina(..)
  , look
  ) where

import Quaazar.Render.Projection ( Projection )
import Quaazar.Scene.Hierarchy ( Instance )
import Quaazar.Scene.World ( World )

data Retina = Retina (Instance Projection) World

look :: Instance Projection -> World -> Retina
look = Retina
