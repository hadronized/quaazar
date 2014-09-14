{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Vertices are points in space with extra information attached. This
-- module exports everything you need to handle vertices and all associated
-- types.
--
-- First, you’ll need to define a *vertex specification*, *vertex format*
-- This is done via 'VertexProto'. A 'VertexFormat' is a bunch of vertex
-- component formats, that describe each component of a vertex (see
-- 'VertexCompFormat'). You have several useful information to provide, such
-- as the type of the component ('VertexCompType') or its semantic
-- ('VertexCompSemantic').
--
-- Once you have a vertex format, you can start building vertices. A
-- single vertex is encoded in a type called 'Vertex', which is a list of
-- all its components ('VertexComp'). That list must be correct regarding
-- the vertex format you previously declared. You have a few combinators
-- to build the 'VertexComp':
--
--   - 'integral' takes a list of 'Int' and builds a 'VertexComp';
--   - 'unsigned' takes a list of 'Word32' and builds a 'VertexComp'
--   - 'floating' takes a list of 'Float' and builds a 'VertexComp';
--
-- For instance:
--
-- @
--     let
--       position = floating [0,0,1]
--       boneID   = unsigned [1]
--       color    = floating [1,1,1,1]
--       vert     = [position,boneID,color] -- :: [VertexComp] ~ Vertex
-- @
--
-- You’ll find useful functions, suck as 'deinterleave' and
-- 'withDeinterleave' you can use to deinterleave vertices in order to
-- optimize/whatever evil you plan to do with ;)
----------------------------------------------------------------------------

module Photon.Core.Vertex (
    -- * Vertex format 
    VertexFormat
  , VertexCompFormat(VertexCompFormat)
  , vcFormatType
  , vcFormatNormalized
  , vcFormatSemantic
  , VertexCompType(..)
  , VertexCompSemantic(..)
  , fromVertexCompSemantic
    -- * Vertex
  , VertexComp
  , mapVertexComp
  , vertexCompSize
  , vertexCompBytes
  , integral -- FIXME: ints
  , unsigned -- FIXME: word32s
  , floating -- FIXME: floats
  , Vertex
  , Vertices(Vertices)
  , verticesVerts
  , verticesFormat
  --, merge
  , deinterleave
  , withDeinterleaved
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad ( forM_, void )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.State ( evalStateT, get, put )
import Data.Aeson
import Data.Aeson.Types ( Parser )
import Data.List ( foldl1' )
import Data.Scientific ( toBoundedInteger )
import Data.Semigroup ( Semigroup(..) )
import Data.Word ( Word8, Word32 )
import Foreign.Marshal.Array ( advancePtr, allocaArray, copyArray )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( sizeOf )
import Numeric.Natural ( Natural )
import qualified Foreign.Marshal.Array as FFI ( withArray )

-- |Vertex component format.
data VertexCompFormat = VertexCompFormat {
    -- |Must the component be normalized?
    _vcFormatNormalized :: Bool
    -- |Semantic of the component.
  , _vcFormatSemantic   :: VertexCompSemantic
    -- |Type of the component.
  , _vcFormatType       :: VertexCompType
  } deriving (Eq,Show)

instance FromJSON VertexCompFormat where
  parseJSON = withObject "vertex component format" $ \o ->
    VertexCompFormat <$> o .: "normalized" <*> o .: "semantic" <*> o .: "type"

-- |Possible vertex component type.
data VertexCompType
  = VInt
  | VInt2
  | VInt3
  | VInt4
  | VUInt
  | VUInt2
  | VUInt3
  | VUInt4
  | VFloat
  | VFloat2
  | VFloat3
  | VFloat4
    deriving (Eq,Show)

instance FromJSON VertexCompType where
  parseJSON = withText "vertex component type" parseVCT
    where
      parseVCT t
        | t == "int"    = return VInt
        | t == "int2"   = return VInt2
        | t == "int3"   = return VInt3
        | t == "int4"   = return VInt4
        | t == "uint"   = return VUInt
        | t == "uint2"  = return VUInt2
        | t == "uint3"  = return VUInt3
        | t == "uint4"  = return VUInt4
        | t == "float"  = return VFloat
        | t == "float2" = return VFloat2
        | t == "float3" = return VFloat3
        | t == "float4" = return VFloat4
        | otherwise     = fail "unknown vertex component type"

-- |Availabe vertex component semantics.
data VertexCompSemantic
  = VSPosition
  | VSNormal
  | VSColor
  | VSUV
  | VSCustom Natural
    deriving (Eq,Show)

instance FromJSON VertexCompSemantic where
  parseJSON =
      withObject "vertex component semantic" $ \o ->
         o .: "name" >>= withText "semantic name" (parseVCC o)
    where
      parseVCC o s
        | s == "position" = return VSPosition
        | s == "normal"   = return VSNormal
        | s == "uv"       = return VSUV
        | s == "custom"   = o .: "value" >>= withScientific "custom semantic" parseCustom
        | otherwise       = fail "unknown vertex component semantic"
      parseCustom =
        maybe (fail "incorrect custom semantic value") testVSCustom . toBoundedInteger
      testVSCustom :: Int -> Parser VertexCompSemantic
      testVSCustom sem
        | sem < 0   = fail "negative semantics are forbbiden"
        | otherwise = return (VSCustom $ fromIntegral sem)

-- |Vertex component. It could be integral, unsigned or floating.
data VertexComp
  = IntegralComp [Int]
  | UnsignedComp [Word32]
  | FloatingComp [Float]
    deriving (Eq,Ord,Show)

instance Semigroup VertexComp where
  IntegralComp a <> IntegralComp b = IntegralComp $ a ++ b
  UnsignedComp a <> UnsignedComp b = UnsignedComp $ a ++ b
  FloatingComp a <> FloatingComp b = FloatingComp $ a ++ b
  _              <> _              = error "types mismatch"

-- |'Vertices' is a bunch of vertices associated to a 'VertexFormat'.
data Vertices = Vertices {
    -- |Vertex format to use with.
    _verticesFormat :: VertexFormat
    -- |True vertices.
  , _verticesVerts  :: [Vertex]
  } deriving (Eq,Show)

-- |A 'Vertex' is simply a list of 'VertexComp'.
type Vertex = [VertexComp]

-- |A 'VertexFormat' is simply a list of 'VertexCompFormat'.
type VertexFormat = [VertexCompFormat]

makeLenses ''VertexCompFormat
makeLenses ''Vertices

-- |Get the integral semantic from a 'VertexCompSemantic'.
fromVertexCompSemantic :: VertexCompSemantic -> Natural
fromVertexCompSemantic vcs = case vcs of
    VSPosition -> 0
    VSNormal   -> 1
    VSColor    -> 2
    VSUV       -> 3
    VSCustom s -> 4 + s
 
-- |Build an integral 'VertexComp'.
integral :: [Int] -> VertexComp
integral = IntegralComp

-- |Build an unsigned 'VertexComp'.
unsigned :: [Word32] -> VertexComp
unsigned = UnsignedComp

-- |Build a floating 'VertexComp'.
floating :: [Float] -> VertexComp
floating = FloatingComp

-- |Fold a vertex component using three functions. Depending of the type of
-- the vertex component, the matching one will be used.
foldVertexComp :: ([Int] -> a)
               -> ([Word32] -> a)
               -> ([Float] -> a)
               -> VertexComp -> a
foldVertexComp i u f vc = case vc of
    IntegralComp x -> i x
    UnsignedComp x -> u x
    FloatingComp x -> f x

-- |Map a function over a vertex component. Depending of the type of the
-- vertex component, the matching one will be used.
mapVertexComp :: ([Int] -> [Int])
              -> ([Word32] -> [Word32])
              -> ([Float] -> [Float])
              -> VertexComp
              -> VertexComp
mapVertexComp i u f =
    foldVertexComp (integral . i) (unsigned . u) (floating . f)

-- |Size of a vertex component. In theory it could be any value, but it’s
-- often 1, 2, 3 or 4.
vertexCompSize :: VertexComp -> Int
vertexCompSize = foldVertexComp length length length

-- |Bytes used by a vertex component.
vertexCompBytes :: VertexComp -> Int
vertexCompBytes =
    foldVertexComp
      ((*) (sizeOf (undefined :: Int)) . length)
      ((*) (sizeOf (undefined :: Word32)) . length)
      ((*) (sizeOf (undefined :: Float)) . length)

-- |Component-component vertex merge.
merge :: Vertex -> Vertex -> Vertex
merge = zipWith (<>)

-- |Deinterleave a list of list of 'VertexComp'. A deinterleaved one is simple:
-- a list of 'VertexComp' can be something like '[floating a, unsigned b']. If
-- you have several values like this one and put them in a list, it yields
-- something like:
--
-- @
--     [
--       [floating a,unsigned b]
--     , [floating c,unsigned d]
--     ]
-- @
--
-- A deinterleaved version of this value joins vertically each part:
--
-- @
--     [floating ac,unsigned bd]
-- @
--
-- This function may fail if types mismatch.
deinterleave :: [Vertex] -> [VertexComp]
deinterleave = foldl1' merge

-- |Expose a '[VertexComp]' as a C pointer. This function is the only way to
-- poke such lists through *FFI*, so you might want to use 'deinterleave' if
-- you want to poke '[[VertexComp]]' (i.e. vertices).
--
-- The function that treats the C pointer takes a @Ptr Word8@ that represents
-- the '[VertexComp]' and its size in bytes.
withDeinterleaved :: [VertexComp] -> (Int -> Ptr Word8 -> IO a) -> IO a
withDeinterleaved v f = do
    allocaArray totalBytes $ \buf -> do
      copyVC buf
      f totalBytes buf
  where
    totalBytes = sum (map vertexCompBytes v)
    copyVC b = void . flip evalStateT b $ do
        forM_ v $ \c -> do
          let bytes = vertexCompBytes c
          p <- get
          put $ p `advancePtr` bytes
          let copy x = FFI.withArray x $ \bufx -> copyArray p (castPtr bufx)  bytes
          lift $ foldVertexComp copy copy copy c 
