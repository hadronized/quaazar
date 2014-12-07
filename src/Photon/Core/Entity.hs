-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Entities are very important. They represent “something” with spatial
-- information attached.
--
-- Up to now, supported spatial information is:
--
--   - *position* ;
--   - *orientation* ;
--   - *scaling*.
----------------------------------------------------------------------------

module Photon.Core.Entity (
    -- * Entity
    Entity(Entity)
  , entityPosition
  , entityOrientation
  , entityScale
    -- * Space information
  , Position
  , Dir
  , Axis
  , Orientation
  , Scale
  , origin3
  , xAxis
  , yAxis
  , zAxis
    -- * Combinators
  , origin
  , move
  , position
  , orient
  , orientation
  , rescale
  , scale
    -- * Reaction
  , EntitySpawned(..)
  , EntityLost(..)
  , EntityEffect(..)
  , changePosition
  , changeOrientation
  , changeScale
  ) where

import Control.Lens
import Linear
import Photon.Core.Effect

-- |An entity is a typed spatial transformation. So far, entities
-- enable the use of three space properties:
--
--   - position;
--   - orientation;
--   - scaling.
data Entity = Entity {
    -- |Entity’s position.
    _entityPosition    :: Position
    -- |Entity’s orientation.
  , _entityOrientation :: Orientation
    -- |Entity’s scale.
  , _entityScale       :: Scale
  } deriving (Eq,Show)

data Scale = Scale {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float deriving (Eq,Ord,Show)

type Position    = V3 Float
type Dir         = V3 Float
type Axis        = V3 Float
type Orientation = Quaternion Float

makeLenses ''Entity

-- |Origin of the R³ basis (0,0,0).
origin3 :: Position
origin3 = V3 0 0 0

-- |Common axis.
xAxis,yAxis,zAxis :: Axis
xAxis = V3 1 0 0
yAxis = V3 0 1 0
zAxis = V3 0 0 1

-- |Origin entity.
origin :: Entity
origin = Entity origin3 (axisAngle (-zAxis) 0) (Scale 1 1 1)

-- |Move an entity along a direction.
move :: Dir -> Entity -> Entity
move dir = entityPosition %~ (+dir)

-- |Position an entity at a given position.
position :: Position -> Entity -> Entity
position = set entityPosition

-- |Orient an entity with another orientation.
orient :: Orientation -> Entity -> Entity
orient o = entityOrientation %~ (normalize . (o*))

-- |Set the orientation of an entity with a given one.
orientation :: Orientation -> Entity -> Entity
orientation = set entityOrientation

-- |Rescale an entity.
rescale :: Scale -> Entity -> Entity
rescale (Scale x' y' z') = entityScale %~ \(Scale x y z) -> Scale (x*x') (y*y') (z*z')

-- |Scale an entity.
scale :: Scale -> Entity -> Entity
scale = set entityScale

data EntitySpawned = EntitySpawned (Managed Entity) deriving (Eq,Show)

data EntityLost = EntityLost (Managed Entity) deriving (Eq,Show)

data EntityEffect
  = PositionChanged (Managed Entity) Position
  | OrientationChanged (Managed Entity) Orientation
  | ScaleChanged (Managed Entity) Scale
    deriving (Eq,Show)

instance EffectfulManage Entity EntitySpawned EntityLost where
  spawned = EntitySpawned
  lost = EntityLost

changePosition :: (Effect EntityEffect m)
               => Managed Entity
               -> (Position -> Position)
               -> m (Managed Entity)
changePosition e f = do
    react (PositionChanged e newPos)
    return (e & managed . entityPosition .~ newPos)
  where newPos = f (e^.managed.entityPosition)

changeOrientation :: (Effect EntityEffect m)
                  => Managed Entity
                  -> (Orientation -> Orientation)
                  -> m (Managed Entity)
changeOrientation e f = do
    react (OrientationChanged e newOrient)
    return (e & managed . entityOrientation .~ newOrient)
  where newOrient = f (e^.managed.entityOrientation)

changeScale :: (Effect EntityEffect m)
            => Managed Entity
            -> (Scale -> Scale)
            -> m (Managed Entity)
changeScale e f = do
    react (ScaleChanged e newScale)
    return (e & managed . entityScale .~ newScale)
  where newScale = f (e^.managed.entityScale)
