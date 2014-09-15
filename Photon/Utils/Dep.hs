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
-- When a value dependes on another, it’s common to see the use of
-- *pointers* or *references*, but this module provides another way – more
-- robust and elegant – to do that.
--
-- When a value `a` depends on a value `b`, you can find a `Dep n b` in
-- `a`. This means that `a` has `b` as dependency, and will represent it
-- as `n`.
--
-- A dependency can be in two states: pending or resolved. When the
-- dependency is pending, no actual `b` value is stored in `a`, only its
-- name `n`. When the dependency is resolved, the `n` value is replaced by
-- the dependency itself, `b`.
----------------------------------------------------------------------------

module Photon.Utils.Dep (
    -- * Dependency
    Dep
  , pending
  , resolve
  ) where

data Dep n a
  = Pending n
  | Resolved a
    deriving (Eq,Functor,Show)

pending :: n -> Dep n a
pending = Pending

resolve :: (n -> Maybe a) -> Dep n a -> Maybe (Dep n a)
resolve _ r@Resolved{} = Just r
resolve lk (Pending n) = fmap Resolved (lk n)
