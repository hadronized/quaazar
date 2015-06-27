#############################################################################
##
## Copyright   : (C) 2015 Dimitri Sabadie
## License     : BSD3
##
## Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
##
############################################################################

bl_info = {
    "name"     : "Export keyframes to Quaazar JSON (.qkfr)"
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

# Dump digits after the 6th one in order to make output less heavy.
def round_(x):
  return round(x,6)

class Action:
  def __init__(self, name, curves):
    self.name = name
    self.curves = curves

class Curve:
  def __init__(self, name, keys):
    self.name = name
    self.keys = keys

class Key:
  def __init__(self, interpolation, t, v):
    self.interpolation = interpolation
    self.index = t
    self.value = v

class QuaazarKeyframesExporter(bpy.types.Operator, ExportHelper):
  """Quaazar Keyframes Exporter Script"""
  bl_idname      = "object.quaazar_keyframes_exporter"
  bl_label       = "Quaazar Keyframes Exporter"
  bl_description = "Export all keyframes from the scene"
  bl_options     = {'REGISTER'}

  filename_ext   = ".qkfr"

  sparse = BoolProperty (
      name        = "Sparse output"
    , description = "Should the output file be sparse?"
    , default     = False
    , )

  def execute(self, context):
    actions = bpy.data.actions
    acts = []
    for action in actions:
      print ("exporting action " + action.name)

      # Check whether we only have LocRotScale group
      if not action.groups['LocRotScale'] or len(action.groups) != 1:
        self.report({'ERROR'}, action.name + " should only have LocRotScale")
        return {}

      # Check that we have 10 channels
      #   · location x
      #   · location y
      #   · location z
      #   · orientation x
      #   · orientation y
      #   · orientation z
      #   · orientation w
      #   · scale x
      #   · scale y
      #   · scale z
      channels = action.groups['LocRotScale'].channels
      if len(channels) != 10:
        self.report({'ERROR'}, action.name + " doesn’t have enough channels")
        return {}

      # Get the curves and the keyframe points.
      locXCurve = channels[0]
      locYCurve = channels[1]
      locZCurve = channels[2]
      rotWCurve = channels[3]
      rotXCurve = channels[4]
      rotYCurve = channels[5]
      rotZCurve = channels[6]
      scaXCurve = channels[7]
      scaYCurve = channels[8]
      scaZCurve = channels[9]
      locXKeys = locXCurve.keyframe_points
      locYKeys = locYCurve.keyframe_points
      locZKeys = locZCurve.keyframe_points
      rotWKeys = rotWCurve.keyframe_points
      rotXKeys = rotXCurve.keyframe_points
      rotYKeys = rotYCurve.keyframe_points
      rotZKeys = rotZCurve.keyframe_points
      scaXKeys = scaXCurve.keyframe_points
      scaYKeys = scaYCurve.keyframe_points
      scaZKeys = scaZCurve.keyframe_points

      # Shrink the location curves into a single curve
      locKeys = []
      for i in range(0, len(locXKeys)):
        p = (round_(locXKeys[i].co.y),round_(locYKeys[i].co.y),round_(locZKeys[i].co.y))
        locKeys.append(Key(locXKeys[i].interpolation, round_(locXKeys[i].co.x), p).__dict__)
      locCurve = Curve('position', locKeys).__dict__

      # Shrink the rotation curves into a single curve
      rotKeys = []
      for i in range(0, len(rotWKeys)):
        o = (round_(rotWKeys[i].co.y),round_(rotXKeys[i].co.y),round_(rotYKeys[i].co.y),round_(rotZKeys[i].co.y))
        rotKeys.append(Key(rotWKeys[i].interpolation, round_(rotWKeys[i].co.x), o).__dict__)
      rotCurve = Curve('orientation', rotKeys).__dict__

      # Shrink the scale curves into a single curve
      scaKeys = []
      for i in range (0, len(scaXKeys)):
        s = (round_(scaXKeys[i].co.y),round_(scaYKeys[i].co.y),round_(scaZKeys[i].co.y))
        scaKeys.append(Key(scaXKeys[i].interpolation, round_(scaXKeys[i].co.x), s).__dict__)
      scaCurve = Curve('scale', scaKeys).__dict__

      # Build the action
      act = Action(action.name, [locCurve,rotCurve,scaCurve]).__dict__
      acts.append(act)

    fp = open(self.filepath, "w")
    i = None
    if self.sparse:
      i = 1
    json.dump(acts, fp, sort_keys=True, indent=i)
    fp.close()
    return {'FINISHED'}

def register():
  bpy.utils.register_class(QuaazarKeyframesExporter)

def unregister():
  bpy.utils.unregister_class(QuaazarKeyframesExporter)

if __name__ == "__main__":
  register()
