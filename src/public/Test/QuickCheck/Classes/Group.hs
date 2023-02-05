{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Group".
--
module Test.QuickCheck.Classes.Group
    (
    -- * Group
      groupLaws

    -- * Abelian
    , abelianLaws
    )
    where

import Prelude

import Data.Function
    ( (&) )
import Data.Group
    ( Abelian, Group (..) )
import Data.Proxy
    ( Proxy (..) )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , NonPositive (..)
    , Property
    , cover
    , forAllShrink
    )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Group.Internal
    ( makeLaw0, makeLaw1, makeLaw2, makeProperty )

--------------------------------------------------------------------------------
-- Group
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Group'.
--
-- Tests the following properties:
--
-- prop> invert mempty == mempty
-- prop> a <> invert a == mempty
-- prop> invert a <> a == mempty
-- prop> a ~~ mempty == a
-- prop> a ~~ a == mempty
-- prop> a ~~ b == a <> invert b
-- prop> n >= 0 ==> pow a n == mconcat (replicate n a)
-- prop> n <= 0 ==> pow a n == invert (mconcat (replicate (abs n) a))
--
groupLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Group a)
    => Proxy a
    -> Laws
groupLaws _ = Laws "Group"
    [ makeLaw0 @a
        "groupLaw_invert_mempty"
        (groupLaw_invert_mempty)
    , makeLaw1 @a
        "groupLaw_invert_mappend_1"
        (groupLaw_invert_mappend_1)
    , makeLaw1 @a
        "groupLaw_invert_mappend_2"
        (groupLaw_invert_mappend_2)
    , makeLaw1 @a
        "groupLaw_subtract_mempty"
        (groupLaw_subtract_mempty)
    , makeLaw1 @a
        "groupLaw_subtract_self"
        (groupLaw_subtract_self)
    , makeLaw2 @a
        "groupLaw_subtract_other"
        (groupLaw_subtract_other)
    , makeLaw1 @a
        "groupLaw_pow_nonNegative"
        (groupLaw_pow_nonNegative)
    , makeLaw1 @a
        "groupLaw_pow_nonPositive"
        (groupLaw_pow_nonPositive)
    ]

groupLaw_invert_mempty
    :: forall a. (Eq a, Group a) => Proxy a -> Property
groupLaw_invert_mempty _ =
    makeProperty
        "invert (mempty @a) == (mempty @a)"
        (invert (mempty @a) == (mempty @a))

groupLaw_invert_mappend_1
    :: (Eq a, Group a) => a -> Property
groupLaw_invert_mappend_1 a =
    makeProperty
        "a <> invert a == mempty"
        (a <> invert a == mempty)

groupLaw_invert_mappend_2
    :: (Eq a, Group a) => a -> Property
groupLaw_invert_mappend_2 a =
    makeProperty
        "invert a <> a == mempty"
        (invert a <> a == mempty)

groupLaw_subtract_mempty
    :: (Eq a, Group a) => a -> Property
groupLaw_subtract_mempty a =
    makeProperty
        "a ~~ mempty == a"
        (a ~~ mempty == a)

groupLaw_subtract_self
    :: (Eq a, Group a) => a -> Property
groupLaw_subtract_self a =
    makeProperty
        "a ~~ a == mempty"
        (a ~~ a == mempty)

groupLaw_subtract_other
    :: (Eq a, Group a) => a -> a -> Property
groupLaw_subtract_other a b =
    makeProperty
        "a ~~ b == a <> invert b"
        (a ~~ b == a <> invert b)

groupLaw_pow_nonNegative
    :: (Eq a, Group a) => a -> Property
groupLaw_pow_nonNegative a =
    forAllShrink (arbitrary @(NonNegative Int)) shrink $ \(NonNegative n) ->
    makeProperty
        "pow a n == mconcat (replicate n a)"
        (pow a n == mconcat (replicate n a))

groupLaw_pow_nonPositive
    :: (Eq a, Group a) => a -> Property
groupLaw_pow_nonPositive a =
    forAllShrink (arbitrary @(NonPositive Int)) shrink $ \(NonPositive n) ->
    makeProperty
        "pow a n == invert (mconcat (replicate (abs n) a))"
        (pow a n == invert (mconcat (replicate (abs n) a)))

--------------------------------------------------------------------------------
-- Abelian
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Abelian'.
--
-- Tests the following property:
--
-- prop> a <> b == b <> a
--
abelianLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Abelian a)
    => Proxy a
    -> Laws
abelianLaws _ = Laws "Abelian"
    [ makeLaw2 @a
        "abelianLaw_commutative"
        (abelianLaw_commutative)
    ]

abelianLaw_commutative
    :: (Eq a, Abelian a) => a -> a -> Property
abelianLaw_commutative a b =
    makeProperty
        "a <> b == b <> a"
        (a <> b == b <> a)
    & cover 1
        ((a /= b) && (a <> b /= a) && (b <> a /= b))
        "(a /= b) && (a <> b /= a) && (b <> a /= b)"
