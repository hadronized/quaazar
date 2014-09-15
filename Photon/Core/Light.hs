-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Lighting is required in order to see non-emissive objects.
--
-- 'Light' exposes the light type. You can find these types of light:
--
--   - 'Omni': omni lights – a.k.a. point lights – are lights that emit in
--     all directions
--
-- Whatever the type of a light, it holds lighting information via a value
-- of type 'LightProperties'.
----------------------------------------------------------------------------

module Photon.Core.Light (
    -- * Light
    Light(..)
  ) where

import Photon.Core.LightProperties

-- |Light. Extra information (cuttoff angle for instance) can be added
-- regarding the type of the light.
data Light
  = Omni LightProperties -- ^ Omni light
    deriving (Eq,Show)
