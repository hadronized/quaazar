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
type Axis     = V3 Float

type Orientation = Quaternion Float

origin :: Position
origin = V3 0 0 0

xAxis,yAxis,zAxis :: Axis
xAxis = V3 1 0 0
yAxis = V3 0 1 0
zAxis = V3 0 0 1

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
