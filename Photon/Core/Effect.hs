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

module Photon.Core.Effect where

import Control.Lens
import Prelude hiding ( drop )

-- |Handle used to uniquely represent managed values.
data H a = H {
    getter :: IO a
  , setter :: a -> IO ()
  }

-- |'Managed a' is a value glued to a /handle/ ('H').
data Managed a = Managed {
    _handle  :: H a
  , _managed :: a
  }

makeLenses ''Managed

-- |A 'Manager m' is a monad with two additional methods: 'manage' and 'drop'.
--
-- 'manage' is used to manage any value. It turns 'a' into 'Managed a'. That is
-- pretty useful for /managers/ that want to track objects.
--
-- 'drop' is used when we want to lose track of a managed value.
class (Monad m) => Manager m where
  manage :: a -> m (Managed a)
  drop   :: Managed a -> m ()

-- |A 'Manager m' monad is able to create 'Managed a' values. However, we could
-- also want to react to such change. Reactions are implemented in a special
-- typeclass (see 'Effect'). In order to adapt to reactions, 'EffectfulManage'
-- represents a type that can be reacted to when 'spawned' and 'lost'.
--
-- 'spawned' takes a brand new managed object and returns the corresponding
-- 's' effect.
--
-- 'lost' takes a recently dropped object and returns the corresponding 'l'
-- effect.
--
-- **Important note**: there’s only one instance of 'EffectulManage' per type.
-- That means you can’t have two pairs of effects for a same type.
class EffectfulManage a s l | a -> s l where
  spawned :: Managed a -> s
  lost    :: Managed a -> l

-- |'Effect e m' is a monad 'm 'with a specific effect 'e'.
class (Monad m) => Effect e m where
  react :: e -> m ()

-- |Effectful manage.
spawn :: (Manager m,EffectfulManage a s l,Effect s m) => a -> m (Managed a)
spawn a = do
  ma <- manage a
  react (spawned ma)
  return ma

-- |Effectful drop.
lose :: (Manager m,EffectfulManage a s l,Effect l m) => Managed a -> m ()
lose ma = drop ma >> react (lost ma)
