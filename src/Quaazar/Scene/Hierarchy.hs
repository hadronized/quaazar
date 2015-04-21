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

module Quaazar.Scene.Hierarchy (
    -- * Hierarchy
    Hierarchy
  , runHierarchy
  , above 
  , below 
    -- * Instances
  , Instance
  , instCarried
  , instTransform
  , instantiate
  ) where

import Control.Monad.Reader ( Reader, ask, local, runReader )
import Data.Semigroup ( (<>) )
import Quaazar.Scene.Transform ( Transform )

-- |A @Hierarchy a@ represents a hierarchical computation yielding a value of
-- type 'a'. Hierarchical computations introduce the concept of hierarchy.
-- A computation that lives below another will be impacted by the computations
-- above it. Computations that live on the same level don’t affect each other.
newtype Hierarchy a = Hierarchy { unHierarchy :: Reader Transform a }
  deriving (Applicative,Functor,Monad)

-- |Extract the value out of a hierarchy.
runHierarchy :: Hierarchy a -> Transform -> a
runHierarchy h = runReader (unHierarchy h)

-- |Get the transform above.
above :: Hierarchy Transform
above = Hierarchy ask

-- |Introduce a new hierarchy that lives below the current one.
below :: Transform -> Hierarchy a -> Hierarchy a
below t = Hierarchy . local (<> t) . unHierarchy

-- |Instances are special values generated in hierarchies. An 'Instance' is
-- always hierarchy-dependent and its value directly depends on two important
-- things:
--
--   - the transforms living in the direct above hierarchies;
--   - the transform of the current hierarchy.
--
-- An 'Instance' simply makes a link between the hierarchy and a given 'a'
-- value.
data Instance a = Instance {
    instCarried   :: a
  , instTransform :: Transform
  } deriving (Eq,Functor,Show)

-- |Instantiate a value in the current 'Hierarchy'. That function is the only
-- way to get instances – i.e. @Instance a@.
instantiate :: a -> Transform -> Hierarchy (Instance a)
instantiate a t = fmap (Instance a . (<> t)) above
