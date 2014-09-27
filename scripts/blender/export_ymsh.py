#############################################################################
##
## Copyright   : (C) 2014 Dimitri Sabadie
## License     : BSD3
##
## Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
##
############################################################################

bl_info = {
    "name"     : "Export mesh to photon JSON (.ymsh)"
  , "author"   : "Dimitri Sabadie"
  , "category" : "Import-Export"
  , "location" : "File > Import-Export"
}

import bpy
from bpy_extras.io_utils import ExportHelper
import json

class PhotonMeshExporter(bpy.types.Operator, ExportHelper):
  """Photon Mesh Exporter Script"""
  bl_idname      = "object.photon_mesh_exporter"
  bl_label       = "Photon Mesh Exporter"
  bl_description = "Export all meshes from the scene into a directory"
  bl_options     = {'REGISTER'}

  filename_ext   = ".ymsh"

  def execute(self, context):
    print("-- ----------------------- --")
    print("-- Photon Mesh JSON Export --")
    meshes = getSceneMeshes()
    for msh in meshes:
      if not hasOnlyTris(msh):
        print("W: '" + msh.name + "' is not elegible to export, please convert quadrangles to triangles")
        continue
      print("I: exporting '" + msh.name + "'")
      phmsh = toPhotonMesh(msh)
      print(phmsh.toJSON())
    print("-- ----------------------- --")
    return {'FINISHED'}

def register():
  bpy.utils.register_class(PhotonMeshExporter)

def unregister():
  bpy.utils.unregister_class(PhotonMeshExporter)

if __name__ == "__main__":
  register()

class PhotonMesh:
  def __init__(self, vs, vgr):
    self.vertices = vs
    self.vgroup   = vgr

  def toJSON(self):
    d = { "vertices" : self.vertices, "vgroup" : self.vgroup }
    return json.dumps(d, sort_keys=True, indent=2)

def getSceneMeshes():
  return [obj.data for obj in bpy.context.scene.objects if obj.type == 'MESH']

def hasOnlyTris(msh):
  for poly in msh.polygons:
    if len(poly.vertices) > 3:
      return False
  return True

def toPhotonMesh(msh):
  vs = []
  vg = []

  # vertices
  for vert in msh.vertices:
    pos = [vert.co[0],vert.co[1],vert.co[2]]
    nor = [vert.normal[0],vert.normal[1],vert.normal[2]]
    vs.append({'position' : pos,'normal' : nor})

  # vertex group
  i = 0
  ll = len(msh.loops)
  while i < ll:
    vg.append([ msh.loops[i].vertex_index
              , msh.loops[i+1].vertex_index
              , msh.loops[i+2].vertex_index
              ])
    i += 3

  return PhotonMesh(vs,vg)
