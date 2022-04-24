{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
-- QuickCheck support for testing instances of Monoid subclasses.
--
-- Please note that this module is experimental.
--
module Test.QuickCheck.Monoid.Subclasses
    ( commutativeLaws
    , leftReductiveLaws
    , rightReductiveLaws
    , reductiveLaws
    , leftCancellativeLaws
    , rightCancellativeLaws
    , cancellativeLaws
    , monoidNullLaws
    )
    where

import Prelude hiding
    ( null )

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust, isNothing )
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
    ( Arbitrary (..), Property, Testable, checkCoverage, cover, property )
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
    [ makeLaw "Basic"
        $ commutativeLaw_basic @a
    ]

commutativeLaw_basic
    :: (Eq a, Commutative a) => a -> a -> Property
commutativeLaw_basic a b =
    a <> b == b <> a
    & cover 5 (a == b) "a == b"
    & cover 5 (a /= b) "a /= b"

--------------------------------------------------------------------------------
-- LeftReductive
--------------------------------------------------------------------------------

leftReductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftReductive a)
    => Proxy a
    -> Laws
leftReductiveLaws _ = Laws "LeftReductive"
    [ makeLaw "isPrefix"
        $ leftReductiveLaw_isPrefix @a
    , makeLaw "isPrefix stripPrefix"
        $ leftReductiveLaw_isPrefix_stripPrefix @a
    , makeLaw "stripPrefix"
        $ leftReductiveLaw_stripPrefix @a
    ]

leftReductiveLaw_isPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix a b =
    a `isPrefixOf` (a <> b)
    & cover 5 (a == b) "a == b"
    & cover 5 (a /= b) "a /= b"

leftReductiveLaw_isPrefix_stripPrefix
    :: LeftReductive a => a -> a -> Property
leftReductiveLaw_isPrefix_stripPrefix a b =
    isPrefixOf a b == isJust (stripPrefix a b)
    & cover 5
        (a `isPrefixOf` b)
        "a `isPrefixOf` b"
    & cover 5
        (not (a `isPrefixOf` b))
        "not (a `isPrefixOf` b)"
    & cover 5
        (isJust (stripPrefix a b))
        "isJust (stripPrefix a b)"
    & cover 5
        (isNothing (stripPrefix a b))
        "isNothing (stripPrefix a b)"

leftReductiveLaw_stripPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix a b =
    maybe b (a <>) (stripPrefix a b) == b
    & cover 5
        (isNothing (stripPrefix a b))
        "isNothing (stripPrefix a b)"
    & cover 5
        (isJust (stripPrefix a b))
        "isJust (stripPrefix a b)"

--------------------------------------------------------------------------------
-- RightReductive
--------------------------------------------------------------------------------

rightReductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightReductive a)
    => Proxy a
    -> Laws
rightReductiveLaws _ = Laws "RightReductive"
    [ makeLaw "isSuffix"
        $ rightReductiveLaw_isSuffix @a
    , makeLaw "isSuffix stripSuffix"
        $ rightReductiveLaw_isSuffix_stripSuffix @a
    , makeLaw "stripSuffix"
        $ rightReductiveLaw_stripSuffix @a
    ]

rightReductiveLaw_isSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix a b =
    b `isSuffixOf` (a <> b)
    & cover 5 (a == b) "a == b"
    & cover 5 (a /= b) "a /= b"

rightReductiveLaw_isSuffix_stripSuffix
    :: RightReductive a => a -> a -> Property
rightReductiveLaw_isSuffix_stripSuffix a b =
    isSuffixOf a b == isJust (stripSuffix a b)
    & cover 5
        (a `isSuffixOf` b)
        "a `isSuffixOf` b"
    & cover 5
        (not (a `isSuffixOf` b))
        "not (a `isSuffixOf` b)"
    & cover 5
        (isJust (stripSuffix a b))
        "isJust (stripSuffix a b)"
    & cover 5
        (isNothing (stripSuffix a b))
        "isNothing (stripSuffix a b)"

rightReductiveLaw_stripSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix a b =
    maybe b (<> a) (stripSuffix a b) == b
    & cover 5
        (isNothing (stripSuffix a b))
        "isNothing (stripSuffix a b)"
    & cover 5
        (isJust (stripSuffix a b))
        "isJust (stripSuffix a b)"

--------------------------------------------------------------------------------
-- Reductive
--------------------------------------------------------------------------------

reductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Reductive a)
    => Proxy a
    -> Laws
reductiveLaws _ = Laws "Reductive"
    [ makeLaw "Equivalence (prefix)"
        $ reductiveLaw_equivalence_prefix @a
    , makeLaw "Equivalence (suffix)"
        $ reductiveLaw_equivalence_suffix @a
    , makeLaw "Inversion (prefix)"
        $ reductiveLaw_inversion_prefix @a
    , makeLaw "Inversion (suffix)"
        $ reductiveLaw_inversion_suffix @a
    ]

reductiveLaw_equivalence_prefix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_prefix a b =
    a </> b == stripPrefix b a
    & cover 5
        (isJust (a </> b))
        "isJust (a </> b)"
    & cover 5
        (isNothing (a </> b))
        "isNothing (a </> b)"
    & cover 5
        (isJust (stripPrefix b a))
        "isJust (stripPrefix b a)"
    & cover 5
        (isNothing (stripPrefix b a))
        "isNothing (stripPrefix b a)"

reductiveLaw_equivalence_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix a b =
    a </> b == stripSuffix b a
    & cover 5
        (isJust (a </> b))
        "isJust (a </> b)"
    & cover 5
        (isNothing (a </> b))
        "isNothing (a </> b)"
    & cover 5
        (isJust (stripSuffix b a))
        "isJust (stripSuffix b a)"
    & cover 5
        (isNothing (stripSuffix b a))
        "isNothing (stripSuffix b a)"

reductiveLaw_inversion_prefix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix a b =
    maybe a (b <>) (a </> b) == a
    & cover 5
        (isJust (a </> b))
        "isJust (a </> b)"
    & cover 5
        (isNothing (a </> b))
        "isNothing (a </> b)"

reductiveLaw_inversion_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix a b =
    maybe a (<> b) (a </> b) == a
    & cover 5
        (isJust (a </> b))
        "isJust (a </> b)"
    & cover 5
        (isNothing (a </> b))
        "isNothing (a </> b)"

--------------------------------------------------------------------------------
-- LeftCancellative
--------------------------------------------------------------------------------

leftCancellativeLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftCancellative a)
    => Proxy a
    -> Laws
leftCancellativeLaws _ = Laws "LeftCancellative"
    [ makeLaw "Cancellation"
        $ leftCancellativeLaw_cancellation @a
    ]

leftCancellativeLaw_cancellation
    :: (Eq a, LeftCancellative a) => a -> a -> Property
leftCancellativeLaw_cancellation a b =
    stripPrefix a (a <> b) == Just b
        & cover 5 (a == b) "a == b"
        & cover 5 (a /= b) "a /= b"

--------------------------------------------------------------------------------
-- RightCancellative
--------------------------------------------------------------------------------

rightCancellativeLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightCancellative a)
    => Proxy a
    -> Laws
rightCancellativeLaws _ = Laws "RightCancellative"
    [ makeLaw "Cancellation"
        $ rightCancellativeLaw_cancellation @a
    ]

rightCancellativeLaw_cancellation
    :: (Eq a, RightCancellative a) => a -> a -> Property
rightCancellativeLaw_cancellation a b =
    stripSuffix b (a <> b) == Just a
        & cover 5 (a == b) "a == b"
        & cover 5 (a /= b) "a /= b"

--------------------------------------------------------------------------------
-- Cancellative
--------------------------------------------------------------------------------

cancellativeLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Cancellative a)
    => Proxy a
    -> Laws
cancellativeLaws _ = Laws "Cancellative"
    [ makeLaw "Cancellation (prefix)"
        $ cancellativeLaw_cancellation_prefix @a
    , makeLaw "Cancellation (suffix)"
        $ cancellativeLaw_cancellation_suffix @a
    ]

cancellativeLaw_cancellation_prefix
    :: (Eq a, Cancellative a) => a -> a -> Property
cancellativeLaw_cancellation_prefix a b =
    (a <> b) </> a == Just b
        & cover 5 (a == b) "a == b"
        & cover 5 (a /= b) "a /= b"

cancellativeLaw_cancellation_suffix
    :: (Eq a, Cancellative a) => a -> a -> Property
cancellativeLaw_cancellation_suffix a b =
    (a <> b) </> b == Just a
        & cover 5 (a == b) "a == b"
        & cover 5 (a /= b) "a /= b"

--------------------------------------------------------------------------------
-- MonoidNull
--------------------------------------------------------------------------------

monoidNullLaws
    :: forall a. (Arbitrary a, Show a, Eq a, MonoidNull a)
    => Proxy a
    -> Laws
monoidNullLaws _ = Laws "MonoidNull"
    [ makeLaw "Basic"
        $ monoidNullLaw_basic @a
    ]

monoidNullLaw_basic
    :: (Eq a, MonoidNull a) => a -> Property
monoidNullLaw_basic a =
    null a == (a == mempty)
        & cover 5 (a == mempty) "a == mempty"
        & cover 5 (a /= mempty) "a /= mempty"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

makeLaw :: Testable t => String -> t -> (String, Property)
makeLaw title p = (title, checkCoverage $ property p)
