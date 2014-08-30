module Photon.Core.Entity where

import Control.Lens
import Data.String ( IsString(..) )
import Data.Vector ( Vector, fromList )
import Linear

-- |An entity is a scene object which is instantiated in space. So
-- far, entities enable the use of three space properties:
--
--   - position;
--   - orientation;
--   - scaling.
data Entity a = Entity {
    -- |
    _entityPosition    :: Position
    -- |
  , _entityOrientation :: Orientation
    -- |
  , _entityScale       :: Scale
    -- |
  , _entityName        :: a
  } deriving (Eq,Functor,Show)

data Scale = Scale {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float deriving (Eq,Ord,Show)

type Position    = V3 Float
type Dir         = V3 Float
type Axis        = V3 Float
type Orientation = Quaternion Float

makeLenses ''Entity

instance (IsString a) => IsString (Entity a) where
  fromString = entity . fromString

-- |Origin of the R³ basis (0,0,0).
origin3 :: Position
origin3 = V3 0 0 0

-- |Common axis.
xAxis,yAxis,zAxis :: Axis
xAxis = V3 1 0 0
yAxis = V3 0 1 0
zAxis = V3 0 0 1

-- |Create a new entity with no space information.
entity :: a -> Entity a
entity = Entity (V3 0 0 0) (axisAngle (-zAxis) 0) (Scale 1 1 1)

-- |Move an entity along a direction.
move :: Dir -> Entity a -> Entity a
move dir = entityPosition %~ (+dir)

-- |Position an entity at a given position.
position :: Position -> Entity a -> Entity a
position = set entityPosition

-- |Orient an entity with another orientation.
orient :: Orientation -> Entity a -> Entity a
orient o = entityOrientation %~ (*o)

-- |Set the orientation of an entity with a given one.
orientation :: Orientation -> Entity a -> Entity a
orientation = set entityOrientation

-- |Rescale an entity.
rescale :: Scale -> Entity a -> Entity a
rescale (Scale x' y' z') = entityScale %~ \(Scale x y z) -> Scale (x*x') (y*y') (z*z')

-- |Scale an entity.
scale :: Scale -> Entity a -> Entity a
scale = set entityScale

-- |Entities are stored in a value of type `Entities`. It gathers all scene
-- entities.
--
-- Up to now, you can access an entity using a lens.
data Entities a = Entities {
    -- |Active camera.
    _camera :: Entity a
    -- |All models.
  , _models :: Vector (Entity a)
    -- |All lights.
  , _lights :: Vector (Entity a)
  } deriving (Eq,Functor,Show)

makeLenses ''Entities

-- |Build entities from lists.
entities :: Entity a -> [Entity a] -> [Entity a] -> Entities a
entities c m l = Entities c (fromList m) (fromList l)
