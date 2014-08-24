module Photon.Core.Scene where

import Control.Applicative
import Control.Lens
import Photon.Core.Light ( Light )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Model ( Model )
import Photon.Core.Projection ( Projection )

-- |Scene AST, used to store a scene representation. Up to now, it gathers
-- information about:
--
--   - camera;
--   - meshes;
--   - models;
--   - lights.
--
data Scene a = Scene {
    -- |
    _sceneCamera :: Projection
    -- |
  , _sceneLights :: [Named a Light]
    -- |
  , _sceneMeshes :: [Named a (Mesh,[Named a Model])]
  } deriving (Eq,Show)

-- |A named value.
data Named n t = Named {
    _name  :: n
  , _named :: t
  } deriving (Functor,Eq,Show)

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
newtype IndexPath = IndexPath { unIndexPath :: [Int] } deriving (Eq,Ord,Show)

makeLenses ''Named
makeLenses ''Scene
makeLenses ''IndexPath

nameIndex :: (Eq n) => n -> [(Int,Named n t)] -> Maybe Int
nameIndex _ [] = Nothing
nameIndex n ((i,x):xs)
    | n == x^.name = Just i
    | otherwise    = nameIndex n xs

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
indexPath sc n = inLight <|> inMesh <|> inModel
  where
    inLight = fmap ip1 (nameIndex n scligs)
    inMesh  = fmap ip1 (nameIndex n scmshs) 
    inModel = Nothing
    scligs = ixed (sc^.sceneLights)
    scmshs = ixed $ map (fmap $ fmap ixed) (sc^.sceneMeshes)
    ixed   = zip [0..]
    ip1 i  = IndexPath [i]
