{-# LANGUAGE OverloadedStrings #-} -- for tests only

module Photon.Core.Entity where

import Data.String ( IsString(..) )
import Control.Applicative ( Applicative(..) )
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

-- |An entity graph is a special structure used to create hierarchies between
-- entities. It uses *nodes* to connect entities to each other and create
-- parent relations.
--
-- `CamNode` is the camera node. It embeds an entity representing a camera
-- and any children is then linked to that camera entity.
--
-- `MdlNode` is the model node. It embeds an entity representing a model and
-- any children is then linked to that model entity.
--
-- `LigNode` is the light node. It embeds an entity representing a light and
-- any children is then linked to that light entity.
--
-- `MidNode` is a special node. It’s used to create empty parents in order to
-- affect several nodes at once. It can be used to achieve a lot of aims. The
-- simplest one is the ability to turn two nodes (i.e. two scene entities) into
-- a rigid one (for instance, a light on a weapon). Feel free to explore
-- possibilities!
data EntityGraph a
  = MidNode            [EntityGraph a]
  | CamNode (Entity a) [EntityGraph a]
  | MdlNode (Entity a) [EntityGraph a]
  | LigNode (Entity a) [EntityGraph a]
    deriving (Eq,Functor,Show)

entities :: EntityGraph String
entities = MidNode
    [
      CamNode "cam0" [LigNode "redLight" []]
    , MdlNode (Translate (V3 1 0 0) "redCube") []
    ]
