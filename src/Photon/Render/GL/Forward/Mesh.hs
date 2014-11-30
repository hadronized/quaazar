-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Photon.Render.OpenGL.Forward.Mesh where

import Control.Lens
import Data.Word ( Word32 )
import Foreign.Storable ( sizeOf )
import Linear ( V2, V3 )
import Numeric.Natural ( Natural )
import Photon.Core.Mesh hiding ( Line, Triangle )
import Photon.Core.Normal
import Photon.Core.Position
import Photon.Core.UV
import Photon.Render.OpenGL.Buffer
import Photon.Render.OpenGL.Primitive
import Photon.Render.OpenGL.VertexArray

data FMesh = FMesh {
    -- |VBO.
    _fmeshVBO    :: Buffer
    -- |IBO.
  , _fmeshIBO    :: Buffer
    -- |VAO.
  , _fmeshVAO    :: VertexArray
    -- |Primitive.
  , _fmeshPrim   :: Primitive
    -- |Vertices number.
  , _fmeshVertNB :: Int
  } deriving (Eq,Show)

makeLenses ''FMesh

-- |OpenGL forward renderer’s `Mesh` representation.
forwardMesh :: Mesh -> IO FMesh
forwardMesh msh = case msh^.meshVertices of
    Interleaved v -> forwardMesh (msh & meshVertices .~ deinterleaved v)
    Deinterleaved vnb positions normals uvs -> do
      vb <- genBuffer
      ib <- genBuffer
      va <- genVertexArray

      -- VBO
      let
        vbytes      = posBytes + normalBytes + uvsBytes
        posBytes    = fromIntegral vnb * sizeOf (undefined :: V3 Float)
        normalBytes = fromIntegral vnb * sizeOf (undefined :: V3 Float)
        uvsBytes    = fromIntegral vnb * uvnb * sizeOf (undefined :: V2 Float)
        uvnb        = if null uvs then 0 else length (head uvs)

      bindBuffer vb ArrayBuffer
      initBuffer ArrayBuffer (fromIntegral vbytes)
      bufferSubData ArrayBuffer 0 (fromIntegral posBytes) (map unPosition positions)
      bufferSubData ArrayBuffer posBytes (fromIntegral normalBytes) (map unNormal normals)
      bufferSubData ArrayBuffer (fromIntegral $ posBytes + normalBytes) (fromIntegral uvsBytes) (concatMap (map unUV) uvs)
      unbindBuffer ArrayBuffer

      -- IBO
      let ibytes = fromIntegral $ verticesNb * sizeOf (undefined :: Word32)

      bindBuffer ib IndexBuffer
      initBuffer IndexBuffer ibytes
      bufferSubData IndexBuffer 0 ibytes inds
      unbindBuffer IndexBuffer

      -- VAO
      bindVertexArray va
      bindBuffer vb ArrayBuffer
      
      -- position
      enableVertexAttrib (fromIntegral positionAttribute)
      vertexAttribPointer (fromIntegral positionAttribute) 3 Floats False 0
      -- normal
      enableVertexAttrib (fromIntegral normalAttribute)
      vertexAttribPointer (fromIntegral normalAttribute) 3 Floats True posBytes
      -- uv 0 -- TODO: support all uvchans
      enableVertexAttrib (fromIntegral uv0Attribute)
      vertexAttribPointer (fromIntegral uv0Attribute) 2 Floats True (posBytes + normalBytes)

      unbindBuffer ArrayBuffer

      bindBuffer ib IndexBuffer
      unbindVertexArray

      unbindBuffer IndexBuffer

      return $ FMesh vb ib va prim verticesNb
  where
    inds          = msh^.meshVGroup.to fromVGroup
    verticesNb    = length inds
    prim          = toGLPrimitive (msh^.meshVGroup)

toGLPrimitive :: VGroup -> Primitive
toGLPrimitive vg = case vg of
    Points{}     -> Point
    Lines{}      -> Line
    Triangles{}  -> Triangle
    SLines{}     -> SLine
    STriangles{} -> STriangle

positionAttribute :: Natural
positionAttribute = 0

normalAttribute :: Natural
normalAttribute = 1

uv0Attribute :: Natural
uv0Attribute = 2
