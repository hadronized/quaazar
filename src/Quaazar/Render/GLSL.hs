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

module Quaazar.Render.GLSL where

import Numeric.Natural ( Natural )

--------------------------------------------------------------------------------
-- GLSL SEMANTICS
declUniform :: Natural -> String -> String
declUniform s n = "layout (location = " ++ show s ++ ") uniform " ++ n ++ ";"

shaderModeSem :: Natural
shaderModeSem = 0

camProjViewSem :: Natural 
camProjViewSem = 1

modelSem :: Natural 
modelSem = 2

eyeSem :: Natural 
eyeSem = 3

ligAmbColSem :: Natural 
ligAmbColSem = 4

ligAmbPowSem :: Natural 
ligAmbPowSem = 5

ligOmniNbSem :: Natural 
ligOmniNbSem = 6

-- Since there’re 6 matrices, they take locations 6, 7, 8, 9, 10 and 11.
ligProjViewsSem :: Natural
ligProjViewsSem = 7

ligPosSem :: Natural
ligPosSem = 13

ligRadSem :: Natural
ligRadSem = 14

ligIRadSem :: Natural
ligIRadSem = 15

shadowmapIndexSem :: Natural
shadowmapIndexSem = 16

--------------------------------------------------------------------------------
-- GLSL BINDING POINTS
declUniformBlock :: Natural -> String -> String
declUniformBlock bp block = "layout (std430,binding = " ++ show bp ++ ") buffer " ++ block ++ ";"

ligOmniSSBOBP :: Natural
ligOmniSSBOBP = 0
