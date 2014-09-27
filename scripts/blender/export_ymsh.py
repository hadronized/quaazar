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
import json

class PhotonMeshExporter(bpy.types.Operator):
  """Photon Mesh Exporter Script"""
  bl_idname      = "object.photon_mesh_exporter"
  bl_label       = "Photon Mesh Exporter"
  bl_description = "Export all meshes from the scene into a directory"
  bl_options     = {'REGISTER'}

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
      print(json.dumps(phmsh.__dict__, indent=2, separators=(',', ': ')))
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

class PhotonVertexGroup:
    def __init__(self, inds):
        self.indices = inds

def getSceneMeshes():
  return [obj.data for obj in bpy.context.scene.objects if obj.type == 'MESH']

def hasOnlyTris(msh):
  for poly in msh.polygons:
    if len(poly.vertices) > 3:
      return False
  return True

def toPhotonMesh(msh):
  vs  = []
  for vert in msh.vertices:
    pos = [vert.co[0],vert.co[1],vert.co[2]]
    nor = [vert.normal[0],vert.normal[1],vert.normal[2]]
    vs.append({'position' : pos,'normal' : nor})
  return PhotonMesh(vs,None)
