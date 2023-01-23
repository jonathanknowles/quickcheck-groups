{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Data.Semigroup.Eq
    ( allUnique
    , canVerifyAllNonNull
    )
    where

import Data.List
    ( nub )

import Data.Foldable as F

allUnique :: (Eq a, Foldable f) => f a -> Bool
allUnique as = length (nub xs) == length xs
  where
    xs = F.toList as

canModify :: (Eq a, Semigroup a) => a -> a -> Bool
a `canModify` b = (||)
    (a `canModifyL` b)
    (a `canModifyR` b)

canModifyL :: (Eq a, Semigroup a) => a -> a -> Bool
a `canModifyL` b = b /= a <> b

canModifyR :: (Eq a, Semigroup a) => a -> a -> Bool
a `canModifyR` b = b /= b <> a

canVerifyAllNonNull :: (Eq a, Semigroup a, Foldable f) => f a -> Bool
canVerifyAllNonNull as = F.all (as `canVerifyNonNull`) as

canVerifyNonNull :: (Eq a, Semigroup a, Foldable f) => f a -> a -> Bool
canVerifyNonNull as a = F.any (a `canModify`) as
