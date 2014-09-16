-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Dependencies between values.
--
-- When a value depends on another, it’s common to see the use of
-- *pointers* or *references*, but this module provides another way – more
-- robust and elegant – to do that.
--
-- When a value 'a' depends on a value 'b', you can find a @Dep n b@ in
-- 'a'. This means that 'a' has 'b' as dependency, and will represent it
-- as 'n'.
--
-- A dependency can be in two states: pending or resolved. When the
-- dependency is pending, no actual 'b' value is stored in 'a', only its
-- name 'n'. When the dependency is resolved, the 'n' value is replaced by
-- the dependency itself, 'b'.
--
-- You can thing of 'Dep' a little bit like 'Either'.
----------------------------------------------------------------------------

module Photon.Utils.Dep (
    -- * Dependency
    Dep
  , pending
  , resolve
  , depName
  , resolved
  , isPending
  , isResolved
    -- * Enforcing
  , enforce
  , enforceResolve
  ) where

import Data.Void ( Void )

-- |Dependency of type 'a' through a name 'n'. A dependency has two
-- states:
--   - pending;
--   - resolved.
data Dep n a
  = Pending n
  | Resolved a
    deriving (Eq,Functor,Show)

-- |Create a pending dependency.
pending :: n -> Dep n a
pending = Pending

-- |Resolve a dependency if possible.
resolve :: (n -> Maybe a) -> Dep n a -> Maybe (Dep n a)
resolve _ r@Resolved{} = Just r
resolve lk (Pending n) = fmap Resolved (lk n)

-- |Get the dependency’s name if it’s pending.
depName :: Dep n a -> Maybe n
depName (Pending n) = Just n
depName _           = Nothing

-- |Get the dependency if it’s resolved.
resolved :: Dep n a -> Maybe a
resolved (Resolved a) = Just a
resolved _            = Nothing

-- |Is a @Dep n a@ pending?
isPending :: Dep n a -> Bool
isPending (Pending _) = True
isPending _           = False

-- |Is a @Dep n a@ resolved?
isResolved :: Dep n a -> Bool
isResolved (Resolved _) = True
isResolved _            = False

-- |Sometimes, when a dependency is resolved, we want to express that
-- through the type system. It’s expressed via a non-constructible
-- name – i.e. 'Void'.
--
-- If the dependency is pending, returns 'Nothing'.
--
-- If you want to combine resolving and enforcing, check the
-- 'enforceResolve' function.
enforce :: Dep n a -> Maybe (Dep Void a)
enforce d = case d of
  Pending  _ -> Nothing
  Resolved a -> Just (Resolved a)

-- |Resolve and enforce at the same time.
enforceResolve :: (n -> Maybe a) -> Dep n a -> Maybe (Dep Void a)
enforceResolve _ (Resolved a) = Just (Resolved a)
enforceResolve lk (Pending n) = fmap Resolved (lk n)
