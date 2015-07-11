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
  def __init__(self, interpolation, t, v, left=None, right=None):
    self.interpolation = interpolation
    self.index = t
    self.value = v
    self.left = left
    self.right = right

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
    , default     = True
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

      # Convert the curves and the keyframe points.
      curves = [
          fromFCurve(channels[0], 'position.x')
        , fromFCurve(channels[1], 'position.y')
        , fromFCurve(channels[2], 'position.z')
        , fromFCurve(channels[3], 'orientation.w')
        , fromFCurve(channels[4], 'orientation.x')
        , fromFCurve(channels[5], 'orientation.y')
        , fromFCurve(channels[6], 'orientation.z')
        , fromFCurve(channels[7], 'scale.x')
        , fromFCurve(channels[8], 'scale.y')
        , fromFCurve(channels[9], 'scale.z')
        ]

      # Build the action
      act = Action(action.name, curves).__dict__
      acts.append(act)

    fp = open(self.filepath, "w")
    i = None
    if self.sparse:
      i = 1
    json.dump(acts, fp, sort_keys=True, indent=i)
    fp.close()
    return {'FINISHED'}

# Convert interpolation mode to match smoothie’s. See
# https://hackage.haskell.org/package/smoothie-0.3.3.2/docs/Data-Spline-Key.html#t:Key.
def adaptInterpolationMode(mode):
  matched = 'linear' # default to linear if mode is not supported
  if mode == 'CONSTANT':
    matched = 'hold'
  elif mode == 'LINEAR':
    matched = 'linear'
  elif mode == 'SINE':
    matched = 'cosine'
  elif mode == 'CUBIC':
    matched = 'cubic-hermite'
  elif mode == 'BEZIER':
    matched = 'bezier'
  return matched

# Given a curve, remove all the extra features we don’t need and yield a
# Curve.
def fromFCurve(fcurve, name):
  keys = []
  for kfp in fcurve.keyframe_points:
      left = None
      right = None
      if kfp.interpolation == 'BEZIER':
        left = list(map(round_,kfp.handle_left))
        right = list(map(round_,kfp.handle_right))
      keys.append(Key(
          adaptInterpolationMode(kfp.interpolation)
        , round_(kfp.co.x)
        , round_(kfp.co.y)
        , left
        , right
        ).__dict__
      )
  return Curve(name, keys).__dict__

def register():
  bpy.utils.register_class(QuaazarKeyframesExporter)

def unregister():
  bpy.utils.unregister_class(QuaazarKeyframesExporter)

if __name__ == "__main__":
  register()
