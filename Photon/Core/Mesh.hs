-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Meshes are the way geometry is built up. A 'Mesh' is a bunch of vertices
-- and a description of faces through a vertices group.
--
-- This module also exports all the required parsers needed to parse a mesh
-- and its components.
----------------------------------------------------------------------------

module Photon.Core.Mesh (
    -- * Mesh
    Mesh(Mesh)
  , meshVertices
  , meshVGroup
    -- * Parsing
  , meshParser
    -- * Re-exported modules
  , module Photon.Core.Vertex
  , module Photon.Core.VGroup
  ) where

import Control.Lens
import Control.Monad ( replicateM, unless )
import Data.List.Split ( chunksOf )
import Data.Map as M ( Map, lookup )
import Photon.Core.Parsing
import Photon.Core.Vertex
import Photon.Core.VGroup

data Mesh = Mesh {
    _meshVertices :: Vertices
  , _meshVGroup   :: VGroup
  } deriving (Eq,Show)

makeLenses ''Mesh

type MeshParser = CharParser VertexProto

meshParser :: Map String VertexProto -> MeshParser Mesh
meshParser vps = do
    proto <- between spaces spaces (protoParser <* blanks <* eol)
    maybe (parserFail $ "invalid vertex protocol: " ++ proto) putState (M.lookup proto vps)
    vproto <- getState
    between spaces spaces (string "@vertices" *> blanks *> eol)
    verts <- many1 (between spaces spaces vcompParser)
    prim <- vgroupSectionParser
    vgroup <- vgroupParser prim
    return $ Mesh (Vertices vproto $ chunksOfVerts vproto verts) vgroup
  where
    chunksOfVerts = chunksOf . length

-- Parse the `proto` keyword and returns its name.
protoParser :: MeshParser String
protoParser = string "proto" *> blanks1 *> identifierName

-- Parse a single `VertexComp`.
vcompParser :: MeshParser VertexComp
vcompParser = do
    vcproto <- fmap head getState
    -- FIXME: rotate must be written in a faster way; it’d require to change
    -- the state of the parser
    modifyState rotate
    case vcproto^.vcProtoType of
      VInt    -> parseIntegral 1
      VInt2   -> parseIntegral 2
      VInt3   -> parseIntegral 3
      VInt4   -> parseIntegral 4
      VUInt   -> parseUnsigned 1
      VUInt2  -> parseUnsigned 2
      VUInt3  -> parseUnsigned 3
      VUInt4  -> parseUnsigned 4
      VFloat  -> parseFloating 1
      VFloat2 -> parseFloating 2
      VFloat3 -> parseFloating 3
      VFloat4 -> parseFloating 4
  where
    parseIntegral = vc integral integralParser
    parseUnsigned = vc unsigned unsignedParser
    parseFloating = vc floating floatingParser
    vc toVC p n = fmap toVC $ replicateM n (between blanks blanks p) <* eol

-- |Parse `VGroup` section.
vgroupSectionParser :: MeshParser String
vgroupSectionParser = string "@vgroup" *> blanks1 *> identifierName <* blanks <* eol

-- |Parse a `VGroup`.
vgroupParser :: String -> MeshParser VGroup
vgroupParser prim
    | prim == "points"     = parsePoints
    | prim == "lines"      = parseLines
    | prim == "slines"     = parseSLines
    | prim == "triangles"  = parseTriangles
    | prim == "striangles" = parseSTriangles
    | otherwise            = parserZero
  where
    parsePoints = fmap Points whole
    parseLines = do
        l <- fmap (chunksOf 2) whole
        unless (even . length $ last l) $ parserFail "odd number of indices!"
        return . Lines $ map (\[a,b] -> Line a b) l
    parseSLines = do
        ind@(a:b:xs) <- whole
        unless (length ind > 2) $
          parserFail "at least two indices are required with stripped lines!"
        return $ SLines a b xs
    parseTriangles = do
        l <- fmap (chunksOf 3) whole
        unless ((==3) . length $ last l) $ parserFail "wrong number of indices!"
        return . Triangles $ map (\[a,b,c] -> Triangle a b c) l
    parseSTriangles = do
        ind@(a:b:c:xs) <- whole
        unless (length ind > 3) $
          parserFail "at least three indices are required with stripped triangles!"
        return $ STriangles a b c xs
    whole = spaces *> sepEndBy1 unsignedParser spaces

-- FIXME: this version is quite slow since it’s O(n)
rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]
