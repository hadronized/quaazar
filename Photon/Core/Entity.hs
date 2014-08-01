module Photon.Core.Entity where

import Control.Applicative ( Applicative(..) )
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
--
-- You can compose them in any way you want. Just keep in mind you shouldn’t
-- use the constructor directly. There’re nice combinators to help you build
-- entities in a lightweight and optimal way. See the **Combinators** part
-- of this documentation for further details.
data Entity a
  = Translate Position (Entity a)
  | Orient Orientation (Entity a)
  | Scale {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float (Entity a)
  | Ent a
    deriving (Eq,Functor,Show)

instance Applicative Entity where
  pure = Ent
  Translate x f <*> a = Translate x (f <*> a)
  Orient x f    <*> a = Orient x (f <*> a)
  Scale x y z f <*> a = Scale x y z (f <*> a)
  Ent f         <*> a = fmap f a

instance Monad Entity where
  return = Ent
  Translate x a >>= f = Translate x (a >>= f)
  Orient x a    >>= f = Orient x (a >>= f)
  Scale x y z a >>= f = Scale x y z (a >>= f)
  Ent a         >>= f = f a

instance (IsString a) => IsString (Entity a) where
  fromString = Ent . fromString

type Position = V3 Float
type Dir      = V3 Float
type Axis     = V3 Float

type Orientation = Quaternion Float

-- |Origin of the R³ basis (0,0,0).
origin3 :: Position
origin3 = V3 0 0 0

-- |Common axis.
xAxis,yAxis,zAxis :: Axis
xAxis = V3 1 0 0
yAxis = V3 0 1 0
zAxis = V3 0 0 1

-- |Map a function inside an entity. You can use that function to pass down
-- a function and reach lower entity layers.
mapEntity :: (Entity a -> Entity a) -> Entity a -> Entity a
mapEntity f e = case e of
    Ent{}          -> f e
    Translate p e' -> Translate p (f e')
    Orient o e'    -> Orient o (f e')
    Scale x y z e' -> Scale x y z (f e')

-- |Create a new entity with no space information.
entity :: a -> Entity a
entity = Ent

-- |Move an entity along a direction.
move :: Dir -> Entity a -> Entity a
move dir e = case e of
    Ent{}          -> Translate dir e
    Translate p e' -> Translate (p+dir) e'
    _              -> mapEntity (move dir) e

-- |Position an entity at a given position.
position :: Position -> Entity a -> Entity a
position pos e = case e of
    Ent{}          -> Translate pos e
    Translate _ e' -> Translate pos e'
    _              -> mapEntity (position pos) e

-- |Orient an entity with another orientation.
orient :: Orientation -> Entity a -> Entity a
orient o e = case e of
    Ent{}        -> Orient o e
    Orient o' e' -> Orient (o * o') e'
    _            -> mapEntity (orient o) e

-- |Set the orientation of an entity with a given one.
orientation :: Orientation -> Entity a -> Entity a
orientation o e = case e of
    Ent{}       -> Orient o e
    Orient _ e' -> Orient o e'
    _           -> mapEntity (orientation o) e

-- |Rescale an entity.
rescale :: Float -> Float -> Float -> Entity a -> Entity a
rescale x y z e = case e of
    Ent{}             -> Scale x y z e
    Scale x' y' z' e' -> Scale (x*x') (y*y') (z*z') e'
    _                 -> mapEntity (rescale x y z) e

-- |Scale an entity.
scale :: Float -> Float -> Float -> Entity a -> Entity a
scale x y z e = case e of
    Ent{}          -> Scale x y z e
    Scale _ _ _ e' -> Scale x y z e'
    _              -> mapEntity (scale x y z) e

-- |Entities are stored in a value of type `Entities`. It gathers all scene
-- entities.
--
-- Up to now, you can access an entity using a lens.
data Entities a = Entities {
    -- |All cameras.
    _cameras :: Vector (Entity a)
    -- |All models.
  , _models  :: Vector (Entity a)
    -- |All lights.
  , _lights  :: Vector (Entity a)
  } deriving (Eq,Show)

makeLenses ''Entities

-- |Build entities from lists.
entities :: [Entity a] -> [Entity a] -> [Entity a] -> Entities a
entities c m l = Entities (fromList c) (fromList m) (fromList l)
