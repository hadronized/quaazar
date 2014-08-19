module Photon.Core.Scene where

import Control.Lens
import Data.Map as M ( fromList, lookup )
import Data.Maybe ( isNothing )
import Photon.Core.Color ( Color )
import Photon.Core.Light ( Light )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Projection ( Projection )

-- |Scene AST, used to store a scene representation. Up to now, it gathers
-- information about:
--
--   - camera;
--   - meshes;
--   - models;
--   - lights.
--
-- See `Top` for further information.
newtype Scene a = Scene [Top a] deriving (Eq,Functor,Show)

-- |Top element of a scene AST. It can be a `Cam`, hosting a named projection,
-- a `Msh`, hosting a named mesh and all models using the mesh or a `Lig` which
-- hosts a named light.
data Top a
  = Cam a Projection
  | Msh a Mesh [Mdl a]
  | Lig a Light
    deriving (Eq,Functor,Show)

-- |Model element of a scene AST. It’s actually a named color.
data Mdl a = Mdl a Color deriving (Eq,Functor,Show)

-- |Names are unique identifiers used to identify objects in a scene. For
-- instance, `Scene String` has `String` names. `Scene Integer` has `Integer`
-- names, and so on. That representation is handy, but it will obviously not
-- fit realtime expectations.
--
-- So comes `IndexPath`, a special name. It’s especially great because it comes
-- with a set of built-in properties. First, it enables you to access scene
-- objects in amortized constant time (*O(1)*). Secondly, any type of name used
-- in a scene can be converted to an `IndexPath`. This is an extremely great
-- property, and means you can use any kind of name you want in your scene, you
-- still have the possibility to optimize the names away and use `IndexPath`es.
--
-- Note: you can’t build `IndexPath` on your own. Feel free to read the
-- documentation of the `indexPath` function for further details.
data IndexPath = IndexPath {
    _ipTop :: Int
  , _ipMdl :: Maybe Int
  } deriving (Eq,Ord,Show)

makeLenses ''IndexPath

-- |Map a name to its `IndexPath` representation. That function should be
-- partially applied in order to get a mapping function
-- (`a -> Maybe IndexPath`):
--
--     indexPath scene
--
-- You can then use that to change all names of your scene at once or change
-- similar structure’s names (see `EntityGraph`).
--
--     fmap (fromJust . indexPath scene) scene
indexPath :: (Ord a) => Scene a -> a -> Maybe IndexPath
indexPath (Scene top) =
    flip M.lookup . fromList . concatMap indexTop $ ixed top
  where
    indexTop (i,t) = case t of
        Cam a _     -> [(a,simpleI i)]
        Lig a _     -> [(a,simpleI i)]
        Msh a _ mdl -> (a,simpleI i) : map (indexMdl i) (ixed mdl)
    indexMdl topi (i,Mdl a _) = (a,IndexPath topi (Just i))
    ixed = zip [0..]
    simpleI i = IndexPath i Nothing

-- Lookup a `Top a` in a `Scene a`.
lookupTop :: Scene a -> IndexPath -> Maybe (Top a)
lookupTop (Scene top) ip = top^?ix (ip^.ipTop)

-- Modify a `Top a` in a `Scene a`.
modifyTop :: Scene a -> IndexPath -> (Top a -> Top a) -> Scene a
modifyTop (Scene top) ip f = Scene $ top & ix (ip^.ipTop) %~ f

-- |Lookup a camera in a scene.
lookupCamera :: Scene a -> IndexPath -> Maybe Projection
lookupCamera sc ip = do
    top <- lookupTop sc ip
    case top of
        Cam _ p -> return p
        _       -> Nothing

-- |Modify a camera in a scene.
modifyCamera :: Scene a -> IndexPath -> (Projection -> Projection) -> Scene a
modifyCamera sc ip f =
    modifyTop sc ip $ \top -> case top of
      Cam a p -> Cam a (f p)
      x       -> x

-- |Lookup a light in a scene.
lookupLight :: Scene a -> IndexPath -> Maybe Light
lookupLight sc ip = do
    top <- lookupTop sc ip
    case top of
        Lig _ l -> return l
        _       -> Nothing

-- |Modify a light in a scene.
modifyLight :: Scene a -> IndexPath -> (Light -> Light) -> Scene a
modifyLight sc ip f =
    modifyTop sc ip $ \top -> case top of
      Lig a l -> Lig a (f l)
      x       -> x

-- |Lookup a mesh in a scene.
lookupMesh :: Scene a -> IndexPath -> Maybe Mesh
lookupMesh sc ip = do
    top <- lookupTop sc ip
    case top of
        Msh _ m _ -> return m
        _         -> Nothing

-- |Modify a mesh in a scene.
modifyMesh :: Scene a -> IndexPath -> (Mesh -> Mesh) -> Scene a
modifyMesh sc ip f =
    case ip^.ipMdl of
      Just _ -> sc
      Nothing ->
        modifyTop sc ip $ \top -> case top of
          Msh a m mdls -> Msh a (f m) mdls
          x            -> x

-- |Lookup a model in a scene.
lookupModel :: Scene a -> IndexPath -> Maybe Color
lookupModel sc ip = do
    mdli <- ip^.ipMdl
    top <- lookupTop sc ip
    case top of
        Msh _ _ mdls -> do
          Mdl _ c <- mdls^?ix mdli
          return c
        _            -> Nothing

-- |Modify a model in a scene.
modifyModel :: Scene a -> IndexPath -> (Color -> Color) -> Scene a
modifyModel sc ip f =
    case ip^.ipMdl of
      Nothing -> sc
      Just mdli -> modifyTop sc ip $ \top -> case top of
        Msh a m mdls -> Msh a m (mdls & ix mdli %~ f')
        x            -> x
  where
    f' (Mdl a c) = Mdl a (f c)
