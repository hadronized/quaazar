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

module Photon.Render.Semantics where

----------------------------------------------------------------------------
-- Light semantics
lightCastShadowsSem :: Int
lightCastShadowsSem = 0

lightColorSem :: Int
lightColorSem = 1

lightPositionSem :: Int
lightPositionSem = 2

lightPowerSem :: Int
lightPowerSem = 3

lightRadiusSem :: Int
lightRadiusSem = 4

lightTypeSem :: Int
lightTypeSem = 5

----------------------------------------------------------------------------
-- Material semantics
materialDiffuseAlbedoSem :: Int
materialDiffuseAlbedoSem = 6

materialShininessSem :: Int
materialShininessSem = 7

materialSpecularAlbedoSem :: Int
materialSpecularAlbedoSem = 8

----------------------------------------------------------------------------
-- Mesh semantics
modelMatrixSem :: Int
modelMatrixSem = 9

----------------------------------------------------------------------------
-- Camera semantics
cameraProjViewSem :: Int
cameraProjViewSem = 10

cameraEyeSem :: Int
cameraEyeSem = 11

cameraForwardSem :: Int
cameraForwardSem = 12
