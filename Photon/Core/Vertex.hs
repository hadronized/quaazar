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
    -- * Parsers
  , vertexFormatParser
  , vertexCompFormatParser
  , vertexCompTypeParser
  , vertexCompSemParser
  ) where

import Control.Lens
import Control.Monad ( forM_, void )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.State ( evalStateT, get, put )
import Data.List ( foldl1' )
import Data.Semigroup ( Semigroup(..) )
import Data.Word ( Word8, Word32 )
import Foreign.Marshal.Array ( advancePtr, allocaArray, copyArray, withArray )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( sizeOf )
import Numeric.Natural ( Natural )
import Photon.Utils.Parsing

-- |Vertex component format.
data VertexCompFormat = VertexCompFormat {
    -- |Must the component be normalized?
    _vcFormatNormalized :: Bool
    -- |Semantic of the component.
  , _vcFormatSemantic   :: VertexCompSemantic
    -- |Type of the component.
  , _vcFormatType       :: VertexCompType
  } deriving (Eq,Show)

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

-- |Availabe vertex component semantics.
data VertexCompSemantic
  = VSPosition
  | VSNormal
  | VSColor
  | VSUV
  | VSCustom Natural
    deriving (Eq,Show)

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
          let copy x = withArray x $ \bufx -> copyArray p (castPtr bufx)  bytes
          lift $ foldVertexComp copy copy copy c 

-- |Vertex format parser.
vertexFormatParser :: CharParser s VertexFormat
vertexFormatParser = many1 $ between spaces blanks vertexCompFormatParser <* (void (many1 eol) <|> eof)

-- |Vertex component format parser.
vertexCompFormatParser :: CharParser s VertexCompFormat
vertexCompFormatParser = do
    sem <- vertexCompSemParser
    void $ between blanks blanks (char ':')
    normalized <- option False $ try (string "normalized" *> blanks1) *> pure True
    t <- vertexCompTypeParser
    return $ VertexCompFormat normalized sem t

-- |Parse a 'VertexCompType'.
vertexCompTypeParser :: CharParser s VertexCompType
vertexCompTypeParser = choice (map buildParser tbl) <?> "vertex component type"
  where
    buildParser (n,t) = try (string n *> pure t)
    tbl =
        [
          ("ivec4",VInt4)
        , ("ivec3",VInt3)
        , ("ivec2",VInt2)
        , ("int",VInt)
        , ("uvec4",VUInt4)
        , ("uvec3",VUInt3)
        , ("uvec2",VUInt2)
        , ("uint",VUInt)
        , ("vec2",VFloat2)
        , ("vec3",VFloat3)
        , ("vec4",VFloat4)
        , ("float",VFloat)
        ]

-- |Parse a 'VertexCompSemantic'.
vertexCompSemParser :: CharParser s VertexCompSemantic
vertexCompSemParser = choice (custom : defined)
  where
    defined =
        map buildParser
          [
            ("position",VSPosition)
          , ("normal",VSNormal)
          , ("color",VSColor)
          , ("uv",VSUV)
          ]
    custom = VSCustom <$> try (string "custom" *> blanks1 *> integralParser)
    buildParser (n,t) = try (string n *> pure t)
