-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Make a better world with a colored-one! This module exports a very
-- straight-forward type, 'Color', which is a 4-float vector.
--
-- > For now, any vector-related function/type uses "linear"’s vectors.
----------------------------------------------------------------------------

module Photon.Core.Color where

import Linear.V4 ( V4 )

-- |A color is a 4-float vector. The four channels are:
--
--   - *red*
--   - *green*
--   - *blue*
--   - *alpha*
type Color = V4 Float
