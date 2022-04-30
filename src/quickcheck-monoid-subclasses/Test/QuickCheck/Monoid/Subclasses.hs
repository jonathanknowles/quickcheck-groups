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
    , leftCancellativeLaws
    , leftReductiveLaws
    , monoidNullLaws
    , monusLaws
    , overlappingGCDMonoidLaws
    , reductiveLaws
    , rightCancellativeLaws
    , rightReductiveLaws
    )
    where

import Prelude hiding
    ( null )

import Data.Maybe
    ( isJust )
import Data.Monoid.Cancellative
    ( OverlappingGCDMonoid (..) )
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
    , RightCancellative
    , RightReductive (..)
    , Reductive (..)
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Testable
    , checkCoverage
    , cover
    , property
    , (==>)
    )
import Test.QuickCheck.Classes
    ( Laws (..) )

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
commutativeLaw_basic a b = property $
    a <> b == b <> a

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
leftReductiveLaw_isPrefix_mappend a b = property $
    a `isPrefixOf` (a <> b)

leftReductiveLaw_isPrefix_stripPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix_stripPrefix a b = property $
    isPrefixOf a b == isJust (stripPrefix a b)

leftReductiveLaw_stripPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix a b = property $
    maybe b (a <>) (stripPrefix a b) == b

leftReductiveLaw_stripPrefix_mappend
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix_mappend a b = property $
    fmap (a <>) (stripPrefix a (a <> b)) == Just (a <> b)

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
rightReductiveLaw_isSuffix_mappend a b = property $
    b `isSuffixOf` (a <> b)

rightReductiveLaw_isSuffix_stripSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix_stripSuffix a b = property $
    isSuffixOf a b == isJust (stripSuffix a b)

rightReductiveLaw_stripSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix a b = property $
    maybe b (<> a) (stripSuffix a b) == b

rightReductiveLaw_stripSuffix_mappend
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix_mappend a b = property $
    fmap (<> b) (stripSuffix b (a <> b)) == Just (a <> b)

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
reductiveLaw_equivalence_prefix a b = property $
    a </> b == stripPrefix b a

reductiveLaw_equivalence_prefix_mappend
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_prefix_mappend a b =
    isJust (stripPrefix a (a <> b))
    ==> reductiveLaw_equivalence_prefix a (a <> b)

reductiveLaw_equivalence_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix a b = property $
    a </> b == stripSuffix b a

reductiveLaw_equivalence_suffix_mappend
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix_mappend a b =
    isJust (stripSuffix b (a <> b))
    ==> reductiveLaw_equivalence_suffix b (a <> b)

reductiveLaw_inversion_prefix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix a b = property $
    maybe a (b <>) (a </> b) == a

reductiveLaw_inversion_prefix_mappend
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix_mappend a b =
    isJust ((a <> b) </> a)
    ==> reductiveLaw_inversion_prefix (a <> b) a

reductiveLaw_inversion_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix a b = property $
    maybe a (<> b) (a </> b) == a

reductiveLaw_inversion_suffix_mappend
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix_mappend a b =
    isJust ((a <> b) </> a)
    ==> reductiveLaw_inversion_suffix (a <> b) a

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
leftCancellativeLaw_cancellation a b = property $
    stripPrefix a (a <> b) == Just b

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
rightCancellativeLaw_cancellation a b = property $
    stripSuffix b (a <> b) == Just a

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
cancellativeLaw_cancellation_prefix a b = property $
    (a <> b) </> a == Just b

cancellativeLaw_cancellation_suffix
    :: (Eq a, Cancellative a) => a -> a -> Property
cancellativeLaw_cancellation_suffix a b = property $
    (a <> b) </> b == Just a

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
overlappingGCDMonoidLaw_overlap_stripPrefixOverlap a b = property $
    overlap a b <> stripPrefixOverlap a b == b

overlappingGCDMonoidLaw_overlap_stripSuffixOverlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_overlap_stripSuffixOverlap a b = property $
    stripSuffixOverlap b a <> overlap a b == a

overlappingGCDMonoidLaw_stripOverlap_overlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_overlap a b = property $
    (\(_, x, _) -> x) (stripOverlap a b) == overlap a b

overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap a b = property $
    (\(_, _, x) -> x) (stripOverlap a b) == stripPrefixOverlap a b

overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap
    :: (Eq a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap a b = property $
    (\(x, _, _) -> x) (stripOverlap a b) == stripSuffixOverlap b a

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
        "monusLaw_stripSuffixOverlap"
        (monusLaw_stripSuffixOverlap)
    ]

monusLaw_stripPrefixOverlap
    :: (Eq a, Monus a) => a -> a -> Property
monusLaw_stripPrefixOverlap a b = property $
    a <\> b == stripPrefixOverlap b a

monusLaw_stripSuffixOverlap
    :: (Eq a, Monus a) => a -> a -> Property
monusLaw_stripSuffixOverlap a b = property $
    a <\> b == stripSuffixOverlap b a

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
monoidNullLaw_basic a = property $
    null a == (a == mempty)

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

makeProperty1 :: (Eq a, Monoid a, Testable t) => (a -> t) -> (a -> Property)
makeProperty1 p a
    = cover  0.1 (a == mempty) "a == mempty"
    $ cover 20.0 (a /= mempty) "a /= mempty"
    $ property $ p a

makeProperty2 :: (Eq a, Testable t) => (a -> a -> t) -> (a -> a -> Property)
makeProperty2 p a b
    = cover  0.1 (a == b) "a == b"
    $ cover 20.0 (a /= b) "a /= b"
    $ property $ p a b
