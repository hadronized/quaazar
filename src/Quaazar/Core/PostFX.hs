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

module Quaazar.Core.PostFX (
    -- * PostFX
    PostFX(..)
  ) where

newtype PostFX = PostFX { unPostFX :: String } deriving (Eq,Show)