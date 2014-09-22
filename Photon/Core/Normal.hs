-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Normal in space is a 3-float vector.
----------------------------------------------------------------------------

module Photon.Core.Normal (
    -- * Normal 
    Normal 
  ) where

import Linear ( V3 )

type Normal = V3 Float
