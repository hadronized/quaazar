-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Quaazar.Render.Mesh where

import Control.Lens
import Control.Monad.Trans ( MonadIO(..) )
import Data.Word ( Word32 )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( sizeOf )
import Linear ( M44, V2, V3 )
import Graphics.Rendering.OpenGL.Raw
import Quaazar.Geometry.Mesh hiding ( Line, Triangle )
import Quaazar.Geometry.Normal
import Quaazar.Geometry.Position
import Quaazar.Geometry.UV
import Quaazar.Render.GL.Buffer
import Quaazar.Render.GL.GLObject
import Quaazar.Render.GL.Primitive
import Quaazar.Render.GL.Shader ( Uniform, (@=) )
import Quaazar.Render.GL.VertexArray
import Quaazar.Render.Semantics
import Quaazar.Render.Transform ( transformMatrix )
import Quaazar.Scene.Transform ( Transform )

data GPUMesh = GPUMesh {
    vertexBuffer :: Buffer
  , indexBuffer  :: Buffer
  , renderMesh   :: Uniform (M44 Float)
                 -> Transform
                 -> IO ()
  }

makeLenses ''GPUMesh

-- |OpenGL 'Mesh' representation.
gpuMesh :: (MonadScoped IO m,MonadIO m) => Mesh -> m GPUMesh
gpuMesh msh = case msh^.meshVertices of
    Interleaved v -> gpuMesh (msh & meshVertices .~ deinterleaved v)
    Deinterleaved vnb positions normals uvs -> do
      [vb,ib] <- genObjects 2
      va <- genObject

      -- VBO
      let
        vbytes      = posBytes + normalBytes + uvsBytes
        posBytes    = fromIntegral vnb * sizeOf (undefined :: V3 Float)
        normalBytes = fromIntegral vnb * sizeOf (undefined :: V3 Float)
        uvsBytes    = fromIntegral vnb * sizeOf (undefined :: V2 Float)

      bindBuffer vb ArrayBuffer
      initBuffer ArrayBuffer (fromIntegral vbytes)
      bufferSubData ArrayBuffer 0 (fromIntegral posBytes) (map unPosition positions)
      bufferSubData ArrayBuffer posBytes (fromIntegral normalBytes) (map unNormal normals)
      bufferSubData ArrayBuffer (fromIntegral $ posBytes + normalBytes) (fromIntegral uvsBytes) (map unUV uvs)
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
      enableVertexAttrib (fromInputSem CoInput)
      vertexAttribPointer (fromInputSem CoInput) 3 Floats False 0
      -- normal
      enableVertexAttrib (fromInputSem NoInput)
      vertexAttribPointer (fromInputSem NoInput) 3 Floats True posBytes

      enableVertexAttrib (fromInputSem UVInput)
      vertexAttribPointer (fromInputSem UVInput) 2 Floats True (posBytes + normalBytes)

      unbindBuffer ArrayBuffer

      bindBuffer ib IndexBuffer
      unbindVertexArray

      unbindBuffer IndexBuffer

      return . GPUMesh vb ib $ \modelU trsf -> do
        modelU @= transformMatrix trsf
        bindVertexArray va
        drawElements prim (fromIntegral verticesNb) 0
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
