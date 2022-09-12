{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Classes.Semigroup.Cancellative
    (
    -- * Commutative
      commutativeLaws

    -- * Cancellative
    , cancellativeLaws
    , leftCancellativeLaws
    , rightCancellativeLaws

    -- * Reductive
    , reductiveLaws
    , leftReductiveLaws
    , rightReductiveLaws
    )
    where

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
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
    ( Arbitrary (..), Property, cover )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Semigroup.Internal
    ( makeLaw2, makeProperty )

--------------------------------------------------------------------------------
-- Cancellative
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Cancellative'.
--
-- Tests the following properties:
--
-- prop> (a <> b) </> a == Just b
-- prop> (a <> b) </> b == Just a
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'leftCancellativeLaws'
-- * 'rightCancellativeLaws'
-- * 'reductiveLaws'
--
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
cancellativeLaw_cancellation_prefix a b =
    makeProperty
        "(a <> b) </> a == Just b"
        ((a <> b) </> a == Just b)

cancellativeLaw_cancellation_suffix
    :: (Eq a, Cancellative a) => a -> a -> Property
cancellativeLaw_cancellation_suffix a b =
    makeProperty
        "(a <> b) </> b == Just a"
        ((a <> b) </> b == Just a)

--------------------------------------------------------------------------------
-- Commutative
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Commutative'.
--
-- Tests the following property:
--
-- prop> a <> b == b <> a
--
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
commutativeLaw_basic a b =
    makeProperty
        "a <> b == b <> a"
        (a <> b == b <> a)
    & cover 1
        ((a /= b) && (a <> b /= a) && (b <> a /= b))
        "(a /= b) && (a <> b /= a) && (b <> a /= b)"

--------------------------------------------------------------------------------
-- LeftCancellative
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'LeftCancellative'.
--
-- Tests the following property:
--
-- prop> stripPrefix a (a <> b) == Just b
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'leftReductiveLaws'
--
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
leftCancellativeLaw_cancellation a b =
    makeProperty
        "stripPrefix a (a <> b) == Just b"
        (stripPrefix a (a <> b) == Just b)

--------------------------------------------------------------------------------
-- LeftReductive
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'LeftReductive'.
--
-- Tests the following properties:
--
-- prop> a `isPrefixOf` (a <> b)
-- prop> isPrefixOf a b == isJust (stripPrefix a b)
-- prop> maybe b (a <>) (stripPrefix a b) == b
--
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
    ]

leftReductiveLaw_isPrefix_mappend
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix_mappend a b =
    makeProperty
        "a `isPrefixOf` (a <> b)"
        (a `isPrefixOf` (a <> b))

leftReductiveLaw_isPrefix_stripPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix_stripPrefix a b =
    makeProperty
        "isPrefixOf a b == isJust (stripPrefix a b)"
        (isPrefixOf a b == isJust (stripPrefix a b))

leftReductiveLaw_stripPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix a b =
    makeProperty
        "maybe b (a <>) (stripPrefix a b) == b"
        (maybe b (a <>) (stripPrefix a b) == b)

--------------------------------------------------------------------------------
-- Reductive
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Reductive'.
--
-- Tests the following properties:
--
-- prop> a </> b == stripPrefix b a
-- prop> a </> b == stripSuffix b a
-- prop> maybe a (b <>) (a </> b) == a
-- prop> maybe a (<> b) (a </> b) == a
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'commutativeLaws'
-- * 'leftReductiveLaws'
-- * 'rightReductiveLaws'
--
reductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Reductive a)
    => Proxy a
    -> Laws
reductiveLaws _ = Laws "Reductive"
    [ makeLaw2 @a
        "reductiveLaw_equivalence_prefix"
        (reductiveLaw_equivalence_prefix)
    , makeLaw2 @a
        "reductiveLaw_equivalence_suffix"
        (reductiveLaw_equivalence_suffix)
    , makeLaw2 @a
        "reductiveLaw_inversion_prefix"
        (reductiveLaw_inversion_prefix)
    , makeLaw2 @a
        "reductiveLaw_inversion_suffix"
        (reductiveLaw_inversion_suffix)
    ]

reductiveLaw_equivalence_prefix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_prefix a b =
    makeProperty
        "a </> b == stripPrefix b a"
        (a </> b == stripPrefix b a)

reductiveLaw_equivalence_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix a b =
    makeProperty
        "a </> b == stripSuffix b a"
        (a </> b == stripSuffix b a)

reductiveLaw_inversion_prefix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix a b =
    makeProperty
        "maybe a (b <>) (a </> b) == a"
        (maybe a (b <>) (a </> b) == a)

reductiveLaw_inversion_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix a b =
    makeProperty
        "maybe a (<> b) (a </> b) == a"
        (maybe a (<> b) (a </> b) == a)

--------------------------------------------------------------------------------
-- RightCancellative
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'RightCancellative'.
--
-- Tests the following property:
--
-- prop> stripSuffix b (a <> b) == Just a
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'rightReductiveLaws'
--
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
rightCancellativeLaw_cancellation a b =
    makeProperty
        "stripSuffix b (a <> b) == Just a"
        (stripSuffix b (a <> b) == Just a)

--------------------------------------------------------------------------------
-- RightReductive
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'RightReductive'.
--
-- Tests the following properties:
--
-- prop> b `isSuffixOf` (a <> b)
-- prop> isSuffixOf a b == isJust (stripSuffix a b)
-- prop> maybe b (<> a) (stripSuffix a b) == b
--
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
    ]

rightReductiveLaw_isSuffix_mappend
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix_mappend a b =
    makeProperty
        "b `isSuffixOf` (a <> b)"
        (b `isSuffixOf` (a <> b))

rightReductiveLaw_isSuffix_stripSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix_stripSuffix a b =
    makeProperty
        "isSuffixOf a b == isJust (stripSuffix a b)"
        (isSuffixOf a b == isJust (stripSuffix a b))

rightReductiveLaw_stripSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix a b =
    makeProperty
        "maybe b (<> a) (stripSuffix a b) == b"
        (maybe b (<> a) (stripSuffix a b) == b)
