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
    -- * GCD
      gcdMonoidLaws
    , leftGCDMonoidLaws
    , rightGCDMonoidLaws
    , overlappingGCDMonoidLaws
    , cancellativeGCDMonoidLaws

    -- * Monus
    , monusLaws

    -- * Null
    , monoidNullLaws

    -- * Positive
    , positiveMonoidLaws

    -- * Group
    , groupLaws
    )
    where

import Prelude hiding
    ( gcd, null )

import Data.Function
    ( (&) )
import Data.Group
    ( Group (..) )
import Data.Maybe
    ( isJust )
import Data.Monoid.Cancellative
    ( LeftGCDMonoid (..), OverlappingGCDMonoid (..), RightGCDMonoid (..) )
import Data.Monoid.GCD
    ( GCDMonoid (..) )
import Data.Monoid.Monus
    ( Monus (..) )
import Data.Monoid.Null
    ( MonoidNull (..), PositiveMonoid )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( Cancellative, LeftReductive (..), Reductive (..), RightReductive (..) )
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
import Test.QuickCheck.Classes.Semigroup.Internal
    ( makeLaw0, makeLaw1, makeLaw2, makeLaw3, makeProperty )

--------------------------------------------------------------------------------
-- CancellativeGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Cancellative' and 'GCDMonoid'.
--
-- Tests the following properties:
--
-- prop> gcd (a <> b) (a <> c) == a <> gcd b c
-- prop> gcd (a <> c) (b <> c) == gcd a b <> c
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'cancellativeLaws'
-- * 'gcdMonoidLaws'
--
cancellativeGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Cancellative a, GCDMonoid a)
    => Proxy a
    -> Laws
cancellativeGCDMonoidLaws _ = Laws "CancellativeGCDMonoid"
    [ makeLaw3 @a
        "cancellativeGCDMonoidLaw_prefix"
        (cancellativeGCDMonoidLaw_prefix)
    , makeLaw3 @a
        "cancellativeGCDMonoidLaw_suffix"
        (cancellativeGCDMonoidLaw_suffix)
    ]

cancellativeGCDMonoidLaw_prefix
    :: (Eq a, Cancellative a, GCDMonoid a) => a -> a -> a -> Property
cancellativeGCDMonoidLaw_prefix a b c =
    makeProperty
        "gcd (a <> b) (a <> c) == a <> gcd b c"
        (gcd (a <> b) (a <> c) == a <> gcd b c)
    & cover 1
        (a /= mempty && gcd b c /= mempty && a /= gcd b c)
        "a /= mempty && gcd b c /= mempty && a /= gcd b c"

cancellativeGCDMonoidLaw_suffix
    :: (Eq a, Cancellative a, GCDMonoid a) => a -> a -> a -> Property
cancellativeGCDMonoidLaw_suffix a b c =
    makeProperty
        "gcd (a <> c) (b <> c) == gcd a b <> c"
        (gcd (a <> c) (b <> c) == gcd a b <> c)
    & cover 1
        (c /= mempty && gcd a b /= mempty && c /= gcd a b)
        "c /= mempty && gcd a b /= mempty && c /= gcd a b"

--------------------------------------------------------------------------------
-- GCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'GCDMonoid'.
--
-- Tests the following properties:
--
-- prop> gcd a b == commonPrefix a b
-- prop> gcd a b == commonSuffix a b
-- prop> isJust (a </> gcd a b)
-- prop> isJust (b </> gcd a b)
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'commutativeLaws'
-- * 'reductiveLaws'
-- * 'leftGCDMonoidLaws'
-- * 'rightGCDMonoidLaws'
-- * 'overlappingGCDMonoidLaws'
--
gcdMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, GCDMonoid a)
    => Proxy a
    -> Laws
gcdMonoidLaws _ = Laws "GCDMonoid"
    [ makeLaw2 @a
        "gcdMonoidLaw_gcd_commonPrefix"
        (gcdMonoidLaw_gcd_commonPrefix)
    , makeLaw2 @a
        "gcdMonoidLaw_gcd_commonSuffix"
        (gcdMonoidLaw_gcd_commonSuffix)
    , makeLaw2 @a
        "gcdMonoidLaw_gcd_reduction_1"
        (gcdMonoidLaw_gcd_reduction_1)
    , makeLaw2 @a
        "gcdMonoidLaw_gcd_reduction_2"
        (gcdMonoidLaw_gcd_reduction_2)
    ]

gcdMonoidLaw_gcd_commonPrefix
    :: (Eq a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_gcd_commonPrefix a b =
    makeProperty
        "gcd a b == commonPrefix a b"
        (gcd a b == commonPrefix a b)

gcdMonoidLaw_gcd_commonSuffix
    :: (Eq a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_gcd_commonSuffix a b =
    makeProperty
        "gcd a b == commonSuffix a b"
        (gcd a b == commonSuffix a b)

gcdMonoidLaw_gcd_reduction_1
    :: (Eq a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_gcd_reduction_1 a b =
    makeProperty
        "isJust (a </> gcd a b)"
        (isJust (a </> gcd a b))

gcdMonoidLaw_gcd_reduction_2
    :: (Eq a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_gcd_reduction_2 a b =
    makeProperty
        "isJust (b </> gcd a b)"
        (isJust (b </> gcd a b))

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
-- LeftGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'LeftGCDMonoid'.
--
-- Tests the following properties:
--
-- prop> stripCommonPrefix a b & \(p, _, _) -> p == commonPrefix a b
-- prop> stripCommonPrefix a b & \(p, x, _) -> p <> x == a
-- prop> stripCommonPrefix a b & \(p, _, x) -> p <> x == b
-- prop> stripCommonPrefix a b & \(p, x, _) -> Just x == stripPrefix p a
-- prop> stripCommonPrefix a b & \(p, _, x) -> Just x == stripPrefix p b
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'leftReductiveLaws'
--
leftGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftGCDMonoid a)
    => Proxy a
    -> Laws
leftGCDMonoidLaws _ = Laws "LeftGCDMonoid"
    [ makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_commonPrefix"
        (leftGCDMonoidLaw_stripCommonPrefix_commonPrefix)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_mappend_1"
        (leftGCDMonoidLaw_stripCommonPrefix_mappend_1)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_mappend_2"
        (leftGCDMonoidLaw_stripCommonPrefix_mappend_2)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1"
        (leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2"
        (leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2)
    ]

leftGCDMonoidLaw_stripCommonPrefix_commonPrefix
    :: (Eq a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_commonPrefix a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, _, _) -> p == commonPrefix a b"
        (stripCommonPrefix a b & \(p, _, _) -> p == commonPrefix a b)

leftGCDMonoidLaw_stripCommonPrefix_mappend_1
    :: (Eq a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_mappend_1 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, x, _) -> p <> x == a"
        (stripCommonPrefix a b & \(p, x, _) -> p <> x == a)

leftGCDMonoidLaw_stripCommonPrefix_mappend_2
    :: (Eq a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_mappend_2 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, _, x) -> p <> x == b"
        (stripCommonPrefix a b & \(p, _, x) -> p <> x == b)

leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1
    :: (Eq a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, x, _) -> Just x == stripPrefix p a"
        (stripCommonPrefix a b & \(p, x, _) -> Just x == stripPrefix p a)

leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2
    :: (Eq a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, _, x) -> Just x == stripPrefix p b"
        (stripCommonPrefix a b & \(p, _, x) -> Just x == stripPrefix p b)

--------------------------------------------------------------------------------
-- MonoidNull
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'MonoidNull'.
--
-- Tests the following property:
--
-- prop> null a == (a == mempty)
--
monoidNullLaws
    :: forall a. (Arbitrary a, Show a, Eq a, MonoidNull a)
    => Proxy a
    -> Laws
monoidNullLaws _ = Laws "MonoidNull"
    [ makeLaw1 @a
        "monoidNullLaw_basic"
        (monoidNullLaw_basic)
    ]

monoidNullLaw_basic
    :: (Eq a, MonoidNull a) => a -> Property
monoidNullLaw_basic a =
    makeProperty
        "null a == (a == mempty)"
        (null a == (a == mempty))

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

--------------------------------------------------------------------------------
-- OverlappingGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'OverlappingGCDMonoid'.
--
-- Tests the following properties:
--
-- prop> overlap a b <> stripPrefixOverlap a b == b
-- prop> stripSuffixOverlap b a <> overlap a b == a
-- prop> stripOverlap a b & \(_, x, _) -> x == overlap a b
-- prop> stripOverlap a b & \(_, _, x) -> x == stripPrefixOverlap a b
-- prop> stripOverlap a b & \(x, _, _) -> x == stripSuffixOverlap b a
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'leftReductiveLaws'
-- * 'rightReductiveLaws'
--
overlappingGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, OverlappingGCDMonoid a)
    => Proxy a
    -> Laws
overlappingGCDMonoidLaws _ = Laws "OverlappingGCDMonoid"
    [ makeLaw2 @a
        "overlappingGCDMonoidLaw_overlap_stripPrefixOverlap"
        (overlappingGCDMonoidLaw_overlap_stripPrefixOverlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_overlap_stripSuffixOverlap"
        (overlappingGCDMonoidLaw_overlap_stripSuffixOverlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_stripOverlap_overlap"
        (overlappingGCDMonoidLaw_stripOverlap_overlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap"
        (overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap"
        (overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap)
    ]

overlappingGCDMonoidLaw_overlap_stripPrefixOverlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_overlap_stripPrefixOverlap a b =
    makeProperty
        "overlap a b <> stripPrefixOverlap a b == b"
        (overlap a b <> stripPrefixOverlap a b == b)

overlappingGCDMonoidLaw_overlap_stripSuffixOverlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_overlap_stripSuffixOverlap a b =
    makeProperty
        "stripSuffixOverlap b a <> overlap a b == a"
        (stripSuffixOverlap b a <> overlap a b == a)

overlappingGCDMonoidLaw_stripOverlap_overlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_overlap a b =
    makeProperty
        "stripOverlap a b & λ(_, x, _) -> x == overlap a b"
        (stripOverlap a b & \(_, x, _) -> x == overlap a b)

overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap a b =
    makeProperty
        "stripOverlap a b & λ(_, _, x) -> x == stripPrefixOverlap a b"
        (stripOverlap a b & \(_, _, x) -> x == stripPrefixOverlap a b)

overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap a b =
    makeProperty
        "stripOverlap a b & λ(x, _, _) -> x == stripSuffixOverlap b a"
        (stripOverlap a b & \(x, _, _) -> x == stripSuffixOverlap b a)

--------------------------------------------------------------------------------
-- PositiveMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'PositiveMonoid'.
--
-- Tests the following properties:
--
-- prop> null (a <> b) == (null a && null b)
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'monoidNullLaws'
--
positiveMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, PositiveMonoid a)
    => Proxy a
    -> Laws
positiveMonoidLaws _ = Laws "PositiveMonoid"
    [ makeLaw2 @a
        "positiveMonoidLaw_fundamental"
        (positiveMonoidLaw_fundamental)
    ]

positiveMonoidLaw_fundamental
    :: (Eq a, PositiveMonoid a) => a -> a -> Property
positiveMonoidLaw_fundamental a b =
    makeProperty
        "null (a <> b) == (null a && null b)"
        (null (a <> b) == (null a && null b))

--------------------------------------------------------------------------------
-- RightGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'RightGCDMonoid'.
--
-- Tests the following properties:
--
-- prop> stripCommonSuffix a b & \(_, _, s) -> s == commonSuffix a b
-- prop> stripCommonSuffix a b & \(x, _, s) -> x <> s == a
-- prop> stripCommonSuffix a b & \(_, x, s) -> x <> s == b
-- prop> stripCommonSuffix a b & \(x, _, s) -> Just x == stripSuffix s a
-- prop> stripCommonSuffix a b & \(_, x, s) -> Just x == stripSuffix s b
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'rightReductiveLaws'
--
rightGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightGCDMonoid a)
    => Proxy a
    -> Laws
rightGCDMonoidLaws _ = Laws "RightGCDMonoid"
    [ makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_commonSuffix"
        (rightGCDMonoidLaw_stripCommonSuffix_commonSuffix)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_mappend_1"
        (rightGCDMonoidLaw_stripCommonSuffix_mappend_1)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_mappend_2"
        (rightGCDMonoidLaw_stripCommonSuffix_mappend_2)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1"
        (rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2"
        (rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2)
    ]

rightGCDMonoidLaw_stripCommonSuffix_commonSuffix
    :: (Eq a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_commonSuffix a b =
    makeProperty
        "stripCommonSuffix a b & λ(_, _, s) -> s == commonSuffix a b"
        (stripCommonSuffix a b & \(_, _, s) -> s == commonSuffix a b)

rightGCDMonoidLaw_stripCommonSuffix_mappend_1
    :: (Eq a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_mappend_1 a b =
    makeProperty
        "stripCommonSuffix a b & λ(x, _, s) -> x <> s == a"
        (stripCommonSuffix a b & \(x, _, s) -> x <> s == a)

rightGCDMonoidLaw_stripCommonSuffix_mappend_2
    :: (Eq a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_mappend_2 a b =
    makeProperty
        "stripCommonSuffix a b & λ(_, x, s) -> x <> s == b"
        (stripCommonSuffix a b & \(_, x, s) -> x <> s == b)

rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1
    :: (Eq a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1 a b =
    makeProperty
        "stripCommonSuffix a b & λ(x, _, s) -> Just x == stripSuffix s a"
        (stripCommonSuffix a b & \(x, _, s) -> Just x == stripSuffix s a)

rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2
    :: (Eq a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2 a b =
    makeProperty
        "stripCommonSuffix a b & λ(_, x, s) -> Just x == stripSuffix s b"
        (stripCommonSuffix a b & \(_, x, s) -> Just x == stripSuffix s b)
