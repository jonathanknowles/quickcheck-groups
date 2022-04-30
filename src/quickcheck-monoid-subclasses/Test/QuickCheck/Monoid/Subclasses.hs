{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
-- QuickCheck support for testing instances of Monoid subclasses.
--
-- Please note that this module is experimental.
--
module Test.QuickCheck.Monoid.Subclasses
    ( cancellativeLaws
    , commutativeLaws
    , gcdMonoidLaws
    , leftCancellativeLaws
    , leftGCDMonoidLaws
    , leftReductiveLaws
    , monoidNullLaws
    , monusLaws
    , overlappingGCDMonoidLaws
    , reductiveLaws
    , rightCancellativeLaws
    , rightGCDMonoidLaws
    , rightReductiveLaws
    )
    where

import Prelude hiding
    ( gcd, null )

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Monoid.Cancellative
    ( LeftGCDMonoid (..), OverlappingGCDMonoid (..), RightGCDMonoid (..) )
import Data.Monoid.GCD
    ( GCDMonoid (..) )
import Data.Monoid.Monus
    ( Monus (..) )
import Data.Monoid.Null
    ( MonoidNull (..) )
import Data.Proxy
    ( Proxy )
import Data.Semigroup.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive (..)
    , Reductive (..)
    , RightCancellative
    , RightReductive (..)
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Testable
    , checkCoverage
    , counterexample
    , cover
    , property
    )
import Test.QuickCheck.Classes
    ( Laws (..) )

--------------------------------------------------------------------------------
-- Cancellative
--------------------------------------------------------------------------------

cancellativeLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Cancellative a)
    => Proxy a
    -> Laws
cancellativeLaws _ = Laws "Cancellative"
    [ makeLaw2 @a
        "cancellativeLaw_cancellation_prefix"
        (cancellativeLaw_cancellation_prefix)
    , makeLaw2 @a
        "cancellativeLaw_cancellation_suffix"
        (cancellativeLaw_cancellation_suffix)
    ]

cancellativeLaw_cancellation_prefix
    :: (Eq a, Cancellative a) => a -> a -> Property
cancellativeLaw_cancellation_prefix a b = makeProperty
    "(a <> b) </> a == Just b"
    ((a <> b) </> a == Just b)

cancellativeLaw_cancellation_suffix
    :: (Eq a, Cancellative a) => a -> a -> Property
cancellativeLaw_cancellation_suffix a b = makeProperty
    "(a <> b) </> b == Just a"
    ((a <> b) </> b == Just a)

--------------------------------------------------------------------------------
-- Commutative
--------------------------------------------------------------------------------

commutativeLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Commutative a)
    => Proxy a
    -> Laws
commutativeLaws _ = Laws "Commutative"
    [ makeLaw2 @a
        "commutativeLaw_basic"
        (commutativeLaw_basic)
    ]

commutativeLaw_basic
    :: (Eq a, Commutative a) => a -> a -> Property
commutativeLaw_basic a b = makeProperty
    "a <> b == b <> a"
    (a <> b == b <> a)

--------------------------------------------------------------------------------
-- GCDMonoid
--------------------------------------------------------------------------------

gcdMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, GCDMonoid a)
    => Proxy a
    -> Laws
gcdMonoidLaws _ = Laws "GCDMonoid"
    [ makeLaw2 @a
        "gcdMonoidLaw_gcd_commonPrefix"
        (gcdMonoidLaw_gcd_commonPrefix)
    , makeLaw3 @a
        "gcdMonoidLaw_gcd_commonPrefix_mconcat"
        (gcdMonoidLaw_gcd_commonPrefix_mconcat)
    , makeLaw2 @a
        "gcdMonoidLaw_gcd_commonSuffix"
        (gcdMonoidLaw_gcd_commonSuffix)
    , makeLaw3 @a
        "gcdMonoidLaw_gcd_commonSuffix_mconcat"
        (gcdMonoidLaw_gcd_commonSuffix_mconcat)
    , makeLaw2 @a
        "gcdMonoidLaw_gcd_reduction_1"
        (gcdMonoidLaw_gcd_reduction_1)
    , makeLaw3 @a
        "gcdMonoidLaw_gcd_reduction_1_mconcat"
        (gcdMonoidLaw_gcd_reduction_1_mconcat)
    , makeLaw2 @a
        "gcdMonoidLaw_gcd_reduction_2"
        (gcdMonoidLaw_gcd_reduction_2)
    , makeLaw3 @a
        "gcdMonoidLaw_gcd_reduction_2_mconcat"
        (gcdMonoidLaw_gcd_reduction_2_mconcat)
    ]

gcdMonoidLaw_gcd_commonPrefix
    :: (Eq a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_gcd_commonPrefix a b = makeProperty
    "gcd a b == commonPrefix a b"
    (gcd a b == commonPrefix a b)

gcdMonoidLaw_gcd_commonPrefix_mconcat
    :: (Eq a, GCDMonoid a) => a -> a -> a -> Property
gcdMonoidLaw_gcd_commonPrefix_mconcat a b c = makeProperty
    "gcd (a <> b) (a <> c) == commonPrefix (a <> b) (a <> c)"
    (gcd (a <> b) (a <> c) == commonPrefix (a <> b) (a <> c))

gcdMonoidLaw_gcd_commonSuffix
    :: (Eq a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_gcd_commonSuffix a b = makeProperty
    "gcd a b == commonSuffix a b"
    (gcd a b == commonSuffix a b)

gcdMonoidLaw_gcd_commonSuffix_mconcat
    :: (Eq a, GCDMonoid a) => a -> a -> a -> Property
gcdMonoidLaw_gcd_commonSuffix_mconcat a b c = makeProperty
    "gcd (a <> c) (b <> c) == commonSuffix (a <> c) (b <> c)"
    (gcd (a <> c) (b <> c) == commonSuffix (a <> c) (b <> c))

gcdMonoidLaw_gcd_reduction_1
    :: (Eq a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_gcd_reduction_1 a b = makeProperty
    "isJust (a </> gcd a b)"
    (isJust (a </> gcd a b))

gcdMonoidLaw_gcd_reduction_1_mconcat
    :: (Eq a, GCDMonoid a) => a -> a -> a -> Property
gcdMonoidLaw_gcd_reduction_1_mconcat a b c = makeProperty
    "isJust ((a <> b) </> gcd (a <> b) (a <> c))"
    (isJust ((a <> b) </> gcd (a <> b) (a <> c)))

gcdMonoidLaw_gcd_reduction_2
    :: (Eq a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_gcd_reduction_2 a b = makeProperty
    "isJust (b </> gcd a b)"
    (isJust (b </> gcd a b))

gcdMonoidLaw_gcd_reduction_2_mconcat
    :: (Eq a, GCDMonoid a) => a -> a -> a -> Property
gcdMonoidLaw_gcd_reduction_2_mconcat a b c = makeProperty
    "isJust ((a <> c) </> gcd (a <> b) (a <> c))"
    (isJust ((a <> c) </> gcd (a <> b) (a <> c)))

--------------------------------------------------------------------------------
-- LeftCancellative
--------------------------------------------------------------------------------

leftCancellativeLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftCancellative a)
    => Proxy a
    -> Laws
leftCancellativeLaws _ = Laws "LeftCancellative"
    [ makeLaw2 @a
        "leftCancellativeLaw_cancellation"
        (leftCancellativeLaw_cancellation)
    ]

leftCancellativeLaw_cancellation
    :: (Eq a, LeftCancellative a) => a -> a -> Property
leftCancellativeLaw_cancellation a b = makeProperty
    "stripPrefix a (a <> b) == Just b"
    (stripPrefix a (a <> b) == Just b)

--------------------------------------------------------------------------------
-- LeftGCDMonoid
--------------------------------------------------------------------------------

leftGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftGCDMonoid a)
    => Proxy a
    -> Laws
leftGCDMonoidLaws _ = Laws "LeftGCDMonoid"
    [ makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_cancellation_1"
        (leftGCDMonoidLaw_stripCommonPrefix_cancellation_1)
    , makeLaw3 @a
        "leftGCDMonoidLaw_stripCommonPrefix_cancellation_1_mappend"
        (leftGCDMonoidLaw_stripCommonPrefix_cancellation_1_mappend)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_cancellation_2"
        (leftGCDMonoidLaw_stripCommonPrefix_cancellation_2)
    , makeLaw3 @a
        "leftGCDMonoidLaw_stripCommonPrefix_cancellation_2_mappend"
        (leftGCDMonoidLaw_stripCommonPrefix_cancellation_2_mappend)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_commonPrefix"
        (leftGCDMonoidLaw_stripCommonPrefix_commonPrefix)
    , makeLaw3 @a
        "leftGCDMonoidLaw_stripCommonPrefix_commonPrefix_mappend"
        (leftGCDMonoidLaw_stripCommonPrefix_commonPrefix_mappend)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1"
        (leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1)
    , makeLaw3 @a
        "leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1_mappend"
        (leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1_mappend)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2"
        (leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2_mappend"
        (leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2_mappend)
    ]

leftGCDMonoidLaw_stripCommonPrefix_cancellation_1
    :: (Eq a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_cancellation_1 a b = makeProperty
    "stripCommonPrefix a b & λ(p,x,_) -> p<>x == a"
    (stripCommonPrefix a b & \(p,x,_) -> p<>x == a)

leftGCDMonoidLaw_stripCommonPrefix_cancellation_1_mappend
    :: (Eq a, LeftGCDMonoid a) => a -> a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_cancellation_1_mappend a b c = makeProperty
    "stripCommonPrefix (a<>b) (a<>c) & λ(p,x,_) -> p<>x == a<>b"
    (stripCommonPrefix (a<>b) (a<>c) & \(p,x,_) -> p<>x == a<>b)

leftGCDMonoidLaw_stripCommonPrefix_cancellation_2
    :: (Eq a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_cancellation_2 a b = makeProperty
    "stripCommonPrefix a b & λ(p,_,x) -> p<>x == b"
    (stripCommonPrefix a b & \(p,_,x) -> p<>x == b)

leftGCDMonoidLaw_stripCommonPrefix_cancellation_2_mappend
    :: (Eq a, LeftGCDMonoid a) => a -> a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_cancellation_2_mappend a b c = makeProperty
    "stripCommonPrefix (a<>b) (a<>c) & λ(p,_,x) -> p<>x == a<>c"
    (stripCommonPrefix (a<>b) (a<>c) & \(p,_,x) -> p<>x == a<>c)

leftGCDMonoidLaw_stripCommonPrefix_commonPrefix
    :: (Eq a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_commonPrefix a b = makeProperty
    "stripCommonPrefix a b & λ(p,_,_) -> p == commonPrefix a b"
    (stripCommonPrefix a b & \(p,_,_) -> p == commonPrefix a b)

leftGCDMonoidLaw_stripCommonPrefix_commonPrefix_mappend
    :: (Eq a, LeftGCDMonoid a) => a -> a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_commonPrefix_mappend a b c = makeProperty
    "stripCommonPrefix (a<>b) (a<>c) & λ(p,_,_) -> p == commonPrefix (a<>b) (a<>c)"
    (stripCommonPrefix (a<>b) (a<>c) & \(p,_,_) -> p == commonPrefix (a<>b) (a<>c))

leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1
    :: (Eq a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1 a b = makeProperty
    "stripCommonPrefix a b & λ(p,x,_) -> Just x == stripPrefix p a"
    (stripCommonPrefix a b & \(p,x,_) -> Just x == stripPrefix p a)

leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1_mappend
    :: (Eq a, LeftGCDMonoid a) => a -> a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1_mappend a b c = makeProperty
    "stripCommonPrefix (a<>b) (a<>c) & λ(p,x,_) -> Just x == stripPrefix p (a<>b)"
    (stripCommonPrefix (a<>b) (a<>c) & \(p,x,_) -> Just x == stripPrefix p (a<>b))

leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2
    :: (Eq a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2 a b = makeProperty
    "stripCommonPrefix a b & λ(p,_,x) -> Just x == stripPrefix p b"
    (stripCommonPrefix a b & \(p,_,x) -> Just x == stripPrefix p b)

leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2_mappend
    :: (Eq a, LeftGCDMonoid a) => a -> a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2_mappend a b c = makeProperty
    "stripCommonPrefix (a<>b) (a<>c) & λ(p,_,x) -> Just x == stripPrefix p (a<>c)"
    (stripCommonPrefix (a<>b) (a<>c) & \(p,_,x) -> Just x == stripPrefix p (a<>c))

--------------------------------------------------------------------------------
-- LeftReductive
--------------------------------------------------------------------------------

leftReductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftReductive a)
    => Proxy a
    -> Laws
leftReductiveLaws _ = Laws "LeftReductive"
    [ makeLaw2 @a
        "leftReductiveLaw_isPrefix_mappend"
        (leftReductiveLaw_isPrefix_mappend)
    , makeLaw2 @a
        "leftReductiveLaw_isPrefix_stripPrefix"
        (leftReductiveLaw_isPrefix_stripPrefix)
    , makeLaw2 @a
        "leftReductiveLaw_stripPrefix"
        (leftReductiveLaw_stripPrefix)
    , makeLaw2 @a
        "leftReductiveLaw_stripPrefix_mappend"
        (leftReductiveLaw_stripPrefix_mappend)
    ]

leftReductiveLaw_isPrefix_mappend
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix_mappend a b = makeProperty
    "a `isPrefixOf` (a <> b)"
    (a `isPrefixOf` (a <> b))

leftReductiveLaw_isPrefix_stripPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix_stripPrefix a b = makeProperty
    "isPrefixOf a b == isJust (stripPrefix a b)"
    (isPrefixOf a b == isJust (stripPrefix a b))

leftReductiveLaw_stripPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix a b = makeProperty
    "maybe b (a <>) (stripPrefix a b) == b"
    (maybe b (a <>) (stripPrefix a b) == b)

leftReductiveLaw_stripPrefix_mappend
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix_mappend a b = makeProperty
    "fmap (a <>) (stripPrefix a (a <> b)) == Just (a <> b)"
    (fmap (a <>) (stripPrefix a (a <> b)) == Just (a <> b))

--------------------------------------------------------------------------------
-- MonoidNull
--------------------------------------------------------------------------------

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
monoidNullLaw_basic a = makeProperty
    "null a == (a == mempty)"
    (null a == (a == mempty))

--------------------------------------------------------------------------------
-- Monus
--------------------------------------------------------------------------------

monusLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Monus a)
    => Proxy a
    -> Laws
monusLaws _ = Laws "Monus"
    [ makeLaw2 @a
        "monusLaw_stripPrefixOverlap"
        (monusLaw_stripPrefixOverlap)
    , makeLaw2 @a
        "monusLaw_stripPrefixOverlap_mappend"
        (monusLaw_stripPrefixOverlap_mappend)
    , makeLaw2 @a
        "monusLaw_stripSuffixOverlap"
        (monusLaw_stripSuffixOverlap)
    , makeLaw2 @a
        "monusLaw_stripSuffixOverlap_mappend"
        (monusLaw_stripSuffixOverlap_mappend)
    ]

monusLaw_stripPrefixOverlap
    :: (Eq a, Monus a) => a -> a -> Property
monusLaw_stripPrefixOverlap a b = makeProperty
    "a <\\> b == stripPrefixOverlap b a"
    (a <\\> b == stripPrefixOverlap b a)
  where
    (<\\>) = (<\>)

monusLaw_stripPrefixOverlap_mappend
    :: (Eq a, Monus a) => a -> a -> Property
monusLaw_stripPrefixOverlap_mappend a b = makeProperty
    "(b <> a) <\\> b == stripPrefixOverlap b (b <> a)"
    ((b <> a) <\\> b == stripPrefixOverlap b (b <> a))
  where
    (<\\>) = (<\>)

monusLaw_stripSuffixOverlap
    :: (Eq a, Monus a) => a -> a -> Property
monusLaw_stripSuffixOverlap a b = makeProperty
    "a <\\> b == stripSuffixOverlap b a"
    (a <\\> b == stripSuffixOverlap b a)
  where
    (<\\>) = (<\>)

monusLaw_stripSuffixOverlap_mappend
    :: (Eq a, Monus a) => a -> a -> Property
monusLaw_stripSuffixOverlap_mappend a b = makeProperty
    "(a <> b) <\\> b == stripSuffixOverlap b (a <> b)"
    ((a <> b) <\\> b == stripSuffixOverlap b (a <> b))
  where
    (<\\>) = (<\>)

--------------------------------------------------------------------------------
-- OverlappingGCDMonoid
--------------------------------------------------------------------------------

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
overlappingGCDMonoidLaw_overlap_stripPrefixOverlap a b = makeProperty
    "overlap a b <> stripPrefixOverlap a b == b"
    (overlap a b <> stripPrefixOverlap a b == b)

overlappingGCDMonoidLaw_overlap_stripSuffixOverlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_overlap_stripSuffixOverlap a b = makeProperty
    "stripSuffixOverlap b a <> overlap a b == a"
    (stripSuffixOverlap b a <> overlap a b == a)

overlappingGCDMonoidLaw_stripOverlap_overlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_overlap a b = makeProperty
    "(λ(_, x, _) -> x) (stripOverlap a b) == overlap a b"
    ((\(_, x, _) -> x) (stripOverlap a b) == overlap a b)

overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap a b = makeProperty
    "(λ(_, _, x) -> x) (stripOverlap a b) == stripPrefixOverlap a b"
    ((\(_, _, x) -> x) (stripOverlap a b) == stripPrefixOverlap a b)

overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap a b = makeProperty
    "(λ(x, _, _) -> x) (stripOverlap a b) == stripSuffixOverlap b a"
    ((\(x, _, _) -> x) (stripOverlap a b) == stripSuffixOverlap b a)

--------------------------------------------------------------------------------
-- Reductive
--------------------------------------------------------------------------------

reductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Reductive a)
    => Proxy a
    -> Laws
reductiveLaws _ = Laws "Reductive"
    [ makeLaw2 @a
        "reductiveLaw_equivalence_prefix"
        (reductiveLaw_equivalence_prefix)
    , makeLaw2 @a
        "reductiveLaw_equivalence_prefix_mappend"
        (reductiveLaw_equivalence_prefix_mappend)
    , makeLaw2 @a
        "reductiveLaw_equivalence_suffix"
        (reductiveLaw_equivalence_suffix)
    , makeLaw2 @a
        "reductiveLaw_equivalence_suffix_mappend"
        (reductiveLaw_equivalence_suffix_mappend)
    , makeLaw2 @a
        "reductiveLaw_inversion_prefix"
        (reductiveLaw_inversion_prefix)
    , makeLaw2 @a
        "reductiveLaw_inversion_prefix_mappend"
        (reductiveLaw_inversion_prefix_mappend)
    , makeLaw2 @a
        "reductiveLaw_inversion_suffix"
        (reductiveLaw_inversion_suffix)
    , makeLaw2 @a
        "reductiveLaw_inversion_suffix_mappend"
        (reductiveLaw_inversion_suffix_mappend)
    ]

reductiveLaw_equivalence_prefix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_prefix a b = makeProperty
    "a </> b == stripPrefix b a"
    (a </> b == stripPrefix b a)

reductiveLaw_equivalence_prefix_mappend
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_prefix_mappend a b = makeProperty
    "(b <> a) </> b == stripPrefix b (b <> a)"
    ((b <> a) </> b == stripPrefix b (b <> a))

reductiveLaw_equivalence_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix a b = makeProperty
    "a </> b == stripSuffix b a"
    (a </> b == stripSuffix b a)

reductiveLaw_equivalence_suffix_mappend
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix_mappend a b = makeProperty
    "(a <> b) </> b == stripSuffix b (a <> b)"
    ((a <> b) </> b == stripSuffix b (a <> b))

reductiveLaw_inversion_prefix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix a b = makeProperty
    "maybe a (b <>) (a </> b) == a"
    (maybe a (b <>) (a </> b) == a)

reductiveLaw_inversion_prefix_mappend
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix_mappend a b = makeProperty
    "fmap (b <>) ((b <> a) </> b) == Just (b <> a)"
    (fmap (b <>) ((b <> a) </> b) == Just (b <> a))

reductiveLaw_inversion_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix a b = makeProperty
    "maybe a (<> b) (a </> b) == a"
    (maybe a (<> b) (a </> b) == a)

reductiveLaw_inversion_suffix_mappend
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix_mappend a b = makeProperty
    "fmap (<> b) ((a <> b) </> b) == Just (a <> b)"
    (fmap (<> b) ((a <> b) </> b) == Just (a <> b))

--------------------------------------------------------------------------------
-- RightCancellative
--------------------------------------------------------------------------------

rightCancellativeLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightCancellative a)
    => Proxy a
    -> Laws
rightCancellativeLaws _ = Laws "RightCancellative"
    [ makeLaw2 @a
        "rightCancellativeLaw_cancellation"
        (rightCancellativeLaw_cancellation)
    ]

rightCancellativeLaw_cancellation
    :: (Eq a, RightCancellative a) => a -> a -> Property
rightCancellativeLaw_cancellation a b = makeProperty
    "stripSuffix b (a <> b) == Just a"
    (stripSuffix b (a <> b) == Just a)

--------------------------------------------------------------------------------
-- RightGCDMonoid
--------------------------------------------------------------------------------

rightGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightGCDMonoid a)
    => Proxy a
    -> Laws
rightGCDMonoidLaws _ = Laws "RightGCDMonoid"
    [ makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_cancellation_1"
        (rightGCDMonoidLaw_stripCommonSuffix_cancellation_1)
    , makeLaw3 @a
        "rightGCDMonoidLaw_stripCommonSuffix_cancellation_1_mappend"
        (rightGCDMonoidLaw_stripCommonSuffix_cancellation_1_mappend)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_cancellation_2"
        (rightGCDMonoidLaw_stripCommonSuffix_cancellation_2)
    , makeLaw3 @a
        "rightGCDMonoidLaw_stripCommonSuffix_cancellation_2_mappend"
        (rightGCDMonoidLaw_stripCommonSuffix_cancellation_2_mappend)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_commonSuffix"
        (rightGCDMonoidLaw_stripCommonSuffix_commonSuffix)
    , makeLaw3 @a
        "rightGCDMonoidLaw_stripCommonSuffix_commonSuffix_mappend"
        (rightGCDMonoidLaw_stripCommonSuffix_commonSuffix_mappend)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1"
        (rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1)
    , makeLaw3 @a
        "rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1_mappend"
        (rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1_mappend)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2"
        (rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2_mappend"
        (rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2_mappend)
    ]

rightGCDMonoidLaw_stripCommonSuffix_cancellation_1
    :: (Eq a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_cancellation_1 a b = makeProperty
    "stripCommonSuffix a b & λ(x,_,s) -> x<>s == a"
    (stripCommonSuffix a b & \(x,_,s) -> x<>s == a)

rightGCDMonoidLaw_stripCommonSuffix_cancellation_1_mappend
    :: (Eq a, RightGCDMonoid a) => a -> a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_cancellation_1_mappend a b c = makeProperty
    "stripCommonSuffix (a<>c) (b<>c) & λ(x,_,s) -> x<>s == a<>c"
    (stripCommonSuffix (a<>c) (b<>c) & \(x,_,s) -> x<>s == a<>c)

rightGCDMonoidLaw_stripCommonSuffix_cancellation_2
    :: (Eq a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_cancellation_2 a b = makeProperty
    "stripCommonSuffix a b & λ(_,x,s) -> x<>s == b"
    (stripCommonSuffix a b & \(_,x,s) -> x<>s == b)

rightGCDMonoidLaw_stripCommonSuffix_cancellation_2_mappend
    :: (Eq a, RightGCDMonoid a) => a -> a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_cancellation_2_mappend a b c = makeProperty
    "stripCommonSuffix (a<>c) (b<>c) & λ(_,x,s) -> x<>s == b<>c"
    (stripCommonSuffix (a<>c) (b<>c) & \(_,x,s) -> x<>s == b<>c)

rightGCDMonoidLaw_stripCommonSuffix_commonSuffix
    :: (Eq a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_commonSuffix a b = makeProperty
    "stripCommonSuffix a b & λ(_,_,s) -> s == commonSuffix a b"
    (stripCommonSuffix a b & \(_,_,s) -> s == commonSuffix a b)

rightGCDMonoidLaw_stripCommonSuffix_commonSuffix_mappend
    :: (Eq a, RightGCDMonoid a) => a -> a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_commonSuffix_mappend a b c = makeProperty
    "stripCommonSuffix (a<>c) (b<>c) & λ(_,_,s) -> s == commonSuffix (a<>c) (b<>c)"
    (stripCommonSuffix (a<>c) (b<>c) & \(_,_,s) -> s == commonSuffix (a<>c) (b<>c))

rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1
    :: (Eq a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1 a b = makeProperty
    "stripCommonSuffix a b & λ(x,_,s) -> Just x == stripSuffix s a"
    (stripCommonSuffix a b & \(x,_,s) -> Just x == stripSuffix s a)

rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1_mappend
    :: (Eq a, RightGCDMonoid a) => a -> a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1_mappend a b c = makeProperty
    "stripCommonSuffix (a<>c) (b<>c) & λ(x,_,s) -> Just x == stripSuffix s (a<>c)"
    (stripCommonSuffix (a<>c) (b<>c) & \(x,_,s) -> Just x == stripSuffix s (a<>c))

rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2
    :: (Eq a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2 a b = makeProperty
    "stripCommonSuffix a b & λ(_,x,s) -> Just x == stripSuffix s b"
    (stripCommonSuffix a b & \(_,x,s) -> Just x == stripSuffix s b)

rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2_mappend
    :: (Eq a, RightGCDMonoid a) => a -> a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2_mappend a b c = makeProperty
    "stripCommonSuffix (a<>c) (b<>c) & λ(_,x,s) -> Just x == stripSuffix s (b<>c)"
    (stripCommonSuffix (a<>c) (b<>c) & \(_,x,s) -> Just x == stripSuffix s (b<>c))

--------------------------------------------------------------------------------
-- RightReductive
--------------------------------------------------------------------------------

rightReductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightReductive a)
    => Proxy a
    -> Laws
rightReductiveLaws _ = Laws "RightReductive"
    [ makeLaw2 @a
        "rightReductiveLaw_isSuffix_mappend"
        (rightReductiveLaw_isSuffix_mappend)
    , makeLaw2 @a
        "rightReductiveLaw_isSuffix_stripSuffix"
        (rightReductiveLaw_isSuffix_stripSuffix)
    , makeLaw2 @a
        "rightReductiveLaw_stripSuffix"
        (rightReductiveLaw_stripSuffix)
    , makeLaw2 @a
        "rightReductiveLaw_stripSuffix_mappend"
        (rightReductiveLaw_stripSuffix_mappend)
    ]

rightReductiveLaw_isSuffix_mappend
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix_mappend a b = makeProperty
    "b `isSuffixOf` (a <> b)"
    (b `isSuffixOf` (a <> b))

rightReductiveLaw_isSuffix_stripSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix_stripSuffix a b = makeProperty
    "isSuffixOf a b == isJust (stripSuffix a b)"
    (isSuffixOf a b == isJust (stripSuffix a b))

rightReductiveLaw_stripSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix a b = makeProperty
    "maybe b (<> a) (stripSuffix a b) == b"
    (maybe b (<> a) (stripSuffix a b) == b)

rightReductiveLaw_stripSuffix_mappend
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix_mappend a b = makeProperty
    "fmap (<> b) (stripSuffix b (a <> b)) == Just (a <> b)"
    (fmap (<> b) (stripSuffix b (a <> b)) == Just (a <> b))

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

makeLaw :: Testable t => String -> t -> (String, Property)
makeLaw title t = (title, checkCoverage $ property t)

makeLaw1
    :: (Arbitrary a, Show a, Eq a, Monoid a)
    => String
    -> (a -> Property)
    -> (String, Property)
makeLaw1 s = makeLaw s . makeProperty1

makeLaw2
    :: (Arbitrary a, Show a, Eq a, Testable t)
    => String
    -> (a -> a -> t)
    -> (String, Property)
makeLaw2 s = makeLaw s . makeProperty2

makeLaw3
    :: (Arbitrary a, Show a, Eq a, Testable t)
    => String
    -> (a -> a -> a -> t)
    -> (String, Property)
makeLaw3 s = makeLaw s . makeProperty3

makeProperty :: Testable t => String -> t -> Property
makeProperty propertyDescription t =
    property t & counterexample counterexampleText
  where
    counterexampleText = unlines
        [ mempty
        , "Property not satisfied:"
        , propertyDescription
        ]

makeProperty1
    :: (Eq a, Monoid a, Testable t) => (a -> t) -> (a -> Property)
makeProperty1 p a
    = cover  0.1 (a == mempty) "a == mempty"
    $ cover 20.0 (a /= mempty) "a /= mempty"
    $ property $ p a

makeProperty2
    :: (Eq a, Testable t) => (a -> a -> t) -> (a -> a -> Property)
makeProperty2 p a b
    = cover  0.1 (a == b) "a == b"
    $ cover 20.0 (a /= b) "a /= b"
    $ property $ p a b

makeProperty3
    :: (Eq a, Testable t) => (a -> a -> a -> t) -> (a -> a -> a -> Property)
makeProperty3 p a b c
    = cover 20.0
        (a /= b && b /= c && c /= a)
        "a /= b && b /= c && c /= a"
    $ property $ p a b c
