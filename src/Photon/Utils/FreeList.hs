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

module Photon.Utils.FreeList (
    -- * Creating free lists
    freeList
  , freeListMin
  , toList
    -- * Using free lists
  , nextFree
  , recycleFree
  ) where

-- |A 'FreeList' tracks free indices of another container. That might be very
-- useful for index leasing when you need to handle resources.
newtype FreeList = FreeList {
    -- |This function turns a 'FreeList' into '[Int]'. Keep in mind that
    -- the list is endless.
    toList :: [Int]
  } deriving (Eq,Ord,Show)

-- |'freeListMin m' builds a 'FreeList' using 'm' as first free value.
freeListMin :: Int -> FreeList
freeListMin m = FreeList [m..]

-- |This function builds a 'FreeList'.
freeList :: FreeList
freeList = freeListMin 0

-- |'nextFree fl' outputs '(freeIndex,nfl)' where 'freeIndex' is the next free
-- index and 'nfl' is the initial 'fl' list with 'freeIndex' removed.
nextFree :: FreeList -> (Int,FreeList)
nextFree (FreeList f) = (head f,FreeList $ tail f)

-- |'recycleFree f fl' recycles the index 'f' into the list 'fl'.
recycleFree :: Int -> FreeList -> FreeList
recycleFree f (FreeList fl) = FreeList (f : fl)
