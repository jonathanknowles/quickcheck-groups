{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides:
--
-- * Support for testing 'Semigroup' subclass instances with QuickCheck.
-- * Reusable properties in the form of 'Laws' definitions.
--
-- In general:
--
-- * Each 'Laws' definition includes properties that relate to __just one__
--   __class__.
-- * Laws for superclasses are __not__ transitively included.
--
-- Therefore, when testing laws for a particular class, you should make sure to
-- also test laws for all superclasses.
--
module Test.QuickCheck.Classes.Semigroup
    (
    -- * Monus
      monusLaws

    -- * Group
    , groupLaws
    )
    where

import Prelude hiding
    ( gcd, null )

import Data.Group
    ( Group (..) )
import Data.Monoid.GCD
    ( OverlappingGCDMonoid (..) )
import Data.Monoid.Monus
    ( Monus (..) )
import Data.Proxy
    ( Proxy (..) )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , NonPositive (..)
    , Property
    , forAllShrink
    )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Semigroup.Internal
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
-- Monus
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Monus'.
--
-- Tests the following properties:
--
-- prop> a <\> b == stripPrefixOverlap b a
-- prop> a <\> b == stripSuffixOverlap b a
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'commutativeLaws'
-- * 'overlappingGCDMonoidLaws'
--
monusLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Monus a)
    => Proxy a
    -> Laws
monusLaws _ = Laws "Monus"
    [ makeLaw2 @a
        "monusLaw_stripPrefixOverlap"
        (monusLaw_stripPrefixOverlap)
    , makeLaw2 @a
        "monusLaw_stripSuffixOverlap"
        (monusLaw_stripSuffixOverlap)
    ]

monusLaw_stripPrefixOverlap
    :: (Eq a, Monus a) => a -> a -> Property
monusLaw_stripPrefixOverlap a b =
    makeProperty
        "a <\\> b == stripPrefixOverlap b a"
        (a <\\> b == stripPrefixOverlap b a)
  where
    (<\\>) = (<\>)

monusLaw_stripSuffixOverlap
    :: (Eq a, Monus a) => a -> a -> Property
monusLaw_stripSuffixOverlap a b =
    makeProperty
        "a <\\> b == stripSuffixOverlap b a"
        (a <\\> b == stripSuffixOverlap b a)
  where
    (<\\>) = (<\>)
