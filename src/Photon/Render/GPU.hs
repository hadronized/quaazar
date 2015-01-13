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

module Photon.Render.GPU (
    -- * GPU pushing
    GPU(..)
  ) where

-- |GPU-representable class.
class GPU a g | g -> a where
  -- |'gpu x' represents 'x' on the /GPU/.
  gpu :: a -> IO g
