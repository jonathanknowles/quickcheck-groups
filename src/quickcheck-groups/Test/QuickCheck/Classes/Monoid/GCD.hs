{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Classes.Monoid.GCD
    ( gcdMonoidLaws
    , leftGCDMonoidLaws
    , rightGCDMonoidLaws
    , cancellativeGCDMonoidLaws
    , overlappingGCDMonoidLaws
    )
    where

import Prelude hiding
    ( gcd )

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Monoid.GCD
    ( GCDMonoid (..)
    , LeftGCDMonoid (..)
    , OverlappingGCDMonoid (..)
    , RightGCDMonoid (..)
    )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( Cancellative, LeftReductive (..), Reductive (..), RightReductive (..) )
import Test.QuickCheck
    ( Arbitrary (..), Property, cover )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Semigroup.Internal
    ( makeLaw2, makeLaw3, makeProperty )

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
