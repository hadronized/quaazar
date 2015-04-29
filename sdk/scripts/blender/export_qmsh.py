#############################################################################
##
## Copyright   : (C) 2015 Dimitri Sabadie
## License     : BSD3
##
## Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
##
############################################################################

bl_info = {
    "name"     : "Export mesh to Quaazar JSON (.qmsh)"
  , "author"   : "Dimitri Sabadie"
  , "category" : "Import-Export"
  , "location" : "File > Import-Export"
}

import bpy
import mathutils
from bpy_extras.io_utils import ExportHelper
from bpy.props import BoolProperty
from math import pi
import json

def round_(x):
  return round(x,6)

class QuaazarMeshExporter(bpy.types.Operator, ExportHelper):
  """Quaazar Mesh Exporter Script"""
  bl_idname      = "object.quaazar_mesh_exporter"
  bl_label       = "Quaazar Mesh Exporter"
  bl_description = "Export all meshes from the scene into a directory"
  bl_options     = {'REGISTER'}

  filename_ext   = ".qmsh"

  sparse = BoolProperty (
      name        = "Sparse output"
    , description = "Should the output file be sparse?"
    , default     = False
    , )

  smoothNormals = BoolProperty (
      name        = "Smooth normals"
    , description = "Smooth normals?"
    , default     = True
    , )

  yUp = BoolProperty (
      name        = "Y axis as up"
    , description = "Rotate the object so that Y is up"
    , default     = True
    , )

  def execute(self, context):
    print("-- ----------------------- --")
    print("-- Quaazar Mesh JSON Export --")
    o = bpy.context.active_object
    if o == None:
      print("E: no mesh selected")
    else:
      msh = o.data
      if self.yUp:
        biasMatrix = mathutils.Matrix.Rotation(pi/2, 3, 'X')
      else:
        biasMatrix = mathutils.Matrix.Identity(3)
      if not hasOnlyTris(msh):
        print("W: '" + msh.name + "' is not elegible to export, please convert quadrangles to triangles")
      else:
        print("I: exporting '" + msh.name + "'")
        phmsh = toQuaazarMesh(msh, self.smoothNormals, biasMatrix)
        fp = open(self.filepath, "w")
        fp.write(phmsh.toJSON(self.sparse))
        fp.close()
    print("-- ----------------------- --")
    return {'FINISHED'}

def register():
  bpy.utils.register_class(QuaazarMeshExporter)

def unregister():
  bpy.utils.unregister_class(QuaazarMeshExporter)

if __name__ == "__main__":
  register()

class QuaazarMesh:
  def __init__(self, vs, vgr):
    self.vertices = vs
    self.vgroup   = vgr

  def toJSON(self, sparse):
    i = 2 if sparse else None
    d = { "vertices" : {"interleaved" : True, "values" : self.vertices}, "vgroup" : { "grouping" : "triangles", "triangles" : self.vgroup } }
    return json.dumps(d, sort_keys=True, indent=i)

# Check whether a mesh has only triangles.
def hasOnlyTris(msh):
  for poly in msh.polygons:
    if len(poly.vertices) > 3:
      return False
  return True

# Build a vertex.
def createVertex(msh, vertID, triID, loopID, smoothNormals, biasMatrix):
  # position
  co = msh.vertices[vertID].co.copy()
  co.rotate(biasMatrix)
  co = list(co)

  # normal
  if smoothNormals:
    no = msh.vertices[vertID].normal.copy()
  else:
    no = msh.polygons[triID].normal.copy()
  no.rotate(mathutils.Matrix.transposed(mathutils.Matrix.inverted(biasMatrix)))
  no = list(no)

  # uv
  uv_layers = msh.uv_layers
  if len(uv_layers) == 1:
    uv = list(uv_layers[0].data[loopID].uv.copy())
    uv[1] = 1 - uv[1]
  else:
    uv = []

  return [co,no,uv]

# Look for a vertex. If it exists, return its ID. Otherwise, return None.
def lookupVertex(vertices, vert):
  try:
    return vertices.index(vert)
  except ValueError:
    return None
    
# Record a new vertex for vertices / indices.
def recordVertex(vertices, indices, vert):
  vid = lookupVertex(vertices, vert)

  if vid == None:
    vid = len(vertices)
    vertices.append(vert)

  return vid
  
def toQuaazarMesh(msh, smoothNormals, biasMatrix):
  tris = msh.polygons
  vertices = []
  indices = []

  for tri in tris:
    triID = tri.index
    loopIDs = list(tri.loop_indices)
    [a,b,c] = tri.vertices
    a_ = createVertex(msh, a, triID, loopIDs[0], smoothNormals, biasMatrix)
    b_ = createVertex(msh, b, triID, loopIDs[1], smoothNormals, biasMatrix)
    c_ = createVertex(msh, c, triID, loopIDs[2], smoothNormals, biasMatrix)
      
    aID = recordVertex(vertices, indices, a_)
    bID = recordVertex(vertices, indices, b_)
    cID = recordVertex(vertices, indices, c_)
    indices.append([aID,bID,cID])

  return QuaazarMesh(vertices,indices)
