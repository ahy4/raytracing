{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module HitableList where

import Prelude
import Vec3
import Ray
import Hitable

newtype HitableList a = HitableList (Hitable a => [a])

instance Hitable a => Hitable (HitableList a) where
  hit (HitableList l) ray tMin tMax = foldl1 folding hitResults
    where
      hitResults = [ hit hitable ray tMin tMax | hitable <- l ]
      folding :: Maybe HitRecord -> Maybe HitRecord -> Maybe HitRecord
      folding Nothing next = next
      folding prev Nothing = prev
      folding (Just prev) (Just next) = if t prev <= t next
        then Just prev
        else Just next
