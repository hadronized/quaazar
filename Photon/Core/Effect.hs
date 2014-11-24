-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- 
----------------------------------------------------------------------------

module Photon.Core.Effect where

import Control.Lens
import Prelude hiding ( drop )

type H = Int

data Managed a = Managed {
    _handle  :: H
  , _managed :: a
  } deriving (Eq,Show)

makeLenses ''Managed

class (Monad m) => Manage m where
  manage :: m H
  drop   :: H -> m ()

class (Monad m) => Effect e m where
  effect :: e -> m ()

spawn :: (Manage m) => a -> m (Managed a)
spawn a = do
  h <- manage
  return (Managed h a)

lose :: (Manage m) => Managed a -> m ()
lose (Managed h _) = drop h
