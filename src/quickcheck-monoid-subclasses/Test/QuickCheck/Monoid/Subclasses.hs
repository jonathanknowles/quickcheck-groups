{-# LANGUAGE FlexibleContexts #-}
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
    ( isJust )
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
    [ makeLaw "Basic"
        $ commutativeLaw_basic @a
    ]

commutativeLaw_basic
    :: (Eq a, Commutative a) => a -> a -> Property
commutativeLaw_basic a b =
    a <> b == b <> a
    & cover 50 (a /= b) "a /= b"

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
    , makeLaw "isPrefix stripPrefix True"
        $ leftReductiveLaw_isPrefix_stripPrefix_True @a
    , makeLaw "isPrefix stripPrefix False"
        $ leftReductiveLaw_isPrefix_stripPrefix_False @a
    , makeLaw "stripPrefix Just"
        $ leftReductiveLaw_stripPrefix_Just @a
    , makeLaw "stripPrefix_Nothing"
        $ leftReductiveLaw_stripPrefix_Nothing @a
    ]

leftReductiveLaw_isPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix a b =
    a `isPrefixOf` (a <> b)
    & cover 50 (a /= b) "a /= b"

leftReductiveLaw_isPrefix_stripPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix_stripPrefix a b = property $
    isPrefixOf a b == isJust (stripPrefix a b)

leftReductiveLaw_isPrefix_stripPrefix_True
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix_stripPrefix_True a b =
    a `isPrefixOf` (a <> b)
        ==> leftReductiveLaw_isPrefix_stripPrefix a (a <> b)
    & cover 50 (a /= b) "a /= b"

leftReductiveLaw_isPrefix_stripPrefix_False
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix_stripPrefix_False a b =
    leftReductiveLaw_isPrefix_stripPrefix (a <> b) b
    & cover 50 (a /= b) "a /= b"

leftReductiveLaw_stripPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix a b = property $
    maybe b (a <>) (stripPrefix a b) == b

leftReductiveLaw_stripPrefix_Just
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix_Just a b =
    isJust (stripPrefix a (a <> b))
        ==> leftReductiveLaw_stripPrefix a (a <> b)
    & cover 50 (a /= b) "a /= b"

leftReductiveLaw_stripPrefix_Nothing
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix_Nothing a b =
    leftReductiveLaw_stripPrefix a b
    & cover 50 (a /= b) "a /= b"

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
    , makeLaw "isSuffix stripSuffix True"
        $ rightReductiveLaw_isSuffix_stripSuffix_True @a
    , makeLaw "isSuffix stripSuffix False"
        $ rightReductiveLaw_isSuffix_stripSuffix_False @a
    , makeLaw "stripSuffix Just"
        $ rightReductiveLaw_stripSuffix_Just @a
    , makeLaw "stripSuffix_Nothing"
        $ rightReductiveLaw_stripSuffix_Nothing @a
    ]

rightReductiveLaw_isSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix a b =
    b `isSuffixOf` (a <> b)
    & cover 50 (a /= b) "a /= b"

rightReductiveLaw_isSuffix_stripSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix_stripSuffix a b = property $
    isSuffixOf a b == isJust (stripSuffix a b)

rightReductiveLaw_isSuffix_stripSuffix_True
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix_stripSuffix_True a b =
    b `isSuffixOf` (a <> b)
        ==> rightReductiveLaw_isSuffix_stripSuffix b (a <> b)
    & cover 50 (a /= b) "a /= b"

rightReductiveLaw_isSuffix_stripSuffix_False
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix_stripSuffix_False a b =
    rightReductiveLaw_isSuffix_stripSuffix a b
    & cover 50 (a /= b) "a /= b"

rightReductiveLaw_stripSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix a b = property $
    maybe b (<> a) (stripSuffix a b) == b

rightReductiveLaw_stripSuffix_Just
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix_Just a b =
    isJust (stripSuffix b (a <> b))
        ==> rightReductiveLaw_stripSuffix a (a <> b)
    & cover 50 (a /= b) "a /= b"

rightReductiveLaw_stripSuffix_Nothing
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix_Nothing a b =
    rightReductiveLaw_stripSuffix a b
    & cover 50 (a /= b) "a /= b"

--------------------------------------------------------------------------------
-- Reductive
--------------------------------------------------------------------------------

reductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Reductive a)
    => Proxy a
    -> Laws
reductiveLaws _ = Laws "Reductive"
    [ makeLaw "Equivalence (prefix) (True)"
        $ reductiveLaw_equivalence_prefix_True @a
    , makeLaw "Equivalence (prefix) (False)"
        $ reductiveLaw_equivalence_prefix_False @a
    , makeLaw "Equivalence (suffix) (True)"
        $ reductiveLaw_equivalence_suffix_True @a
    , makeLaw "Equivalence (suffix) (False)"
        $ reductiveLaw_equivalence_suffix_False @a
    , makeLaw "Inversion (prefix) (Just)"
        $ reductiveLaw_inversion_prefix_Just @a
    , makeLaw "Inversion (prefix) (Nothing)"
        $ reductiveLaw_inversion_prefix_Nothing @a
    , makeLaw "Inversion (suffix) (Just)"
        $ reductiveLaw_inversion_suffix_Just @a
    , makeLaw "Inversion (suffix) (Nothing)"
        $ reductiveLaw_inversion_suffix_Nothing @a
    ]

reductiveLaw_equivalence_prefix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_prefix a b = property $
    a </> b == stripPrefix b a

reductiveLaw_equivalence_prefix_True
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_prefix_True a b =
    a `isPrefixOf` (a <> b)
        ==> reductiveLaw_equivalence_prefix a (a <> b)
    & cover 50 (a /= b) "a /= b"

reductiveLaw_equivalence_prefix_False
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_prefix_False a b =
    reductiveLaw_equivalence_prefix a b
    & cover 50 (a /= b) "a /= b"

reductiveLaw_equivalence_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix a b = property $
    a </> b == stripSuffix b a

reductiveLaw_equivalence_suffix_True
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix_True a b =
    b `isSuffixOf` (a <> b)
        ==> reductiveLaw_equivalence_suffix b (a <> b)
    & cover 50 (a /= b) "a /= b"

reductiveLaw_equivalence_suffix_False
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix_False a b =
    reductiveLaw_equivalence_suffix a b
    & cover 50 (a /= b) "a /= b"

reductiveLaw_inversion_prefix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix a b = property $
    maybe a (b <>) (a </> b) == a

reductiveLaw_inversion_prefix_Just
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix_Just a b =
    isJust ((a <> b) </> a)
        ==> reductiveLaw_inversion_prefix (a <> b) a
    & cover 50 (a /= b) "a /= b"

reductiveLaw_inversion_prefix_Nothing
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix_Nothing a b =
    reductiveLaw_inversion_prefix a b
    & cover 50 (a /= b) "a /= b"

reductiveLaw_inversion_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix a b = property $
    maybe a (<> b) (a </> b) == a

reductiveLaw_inversion_suffix_Just
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix_Just a b =
    isJust ((a <> b) </> a)
        ==> reductiveLaw_inversion_suffix (a <> b) a
    & cover 50 (a /= b) "a /= b"

reductiveLaw_inversion_suffix_Nothing
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix_Nothing a b =
    reductiveLaw_inversion_suffix a b
    & cover 50 (a /= b) "a /= b"

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
leftCancellativeLaw_cancellation a b = property $
    stripPrefix a (a <> b) == Just b
    & cover 50 (a /= b) "a /= b"

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
rightCancellativeLaw_cancellation a b = property $
    stripSuffix b (a <> b) == Just a
    & cover 50 (a /= b) "a /= b"

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
cancellativeLaw_cancellation_prefix a b = property $
    (a <> b) </> a == Just b
    & cover 50 (a /= b) "a /= b"

cancellativeLaw_cancellation_suffix
    :: (Eq a, Cancellative a) => a -> a -> Property
cancellativeLaw_cancellation_suffix a b =
    (a <> b) </> b == Just a
    & cover 50 (a /= b) "a /= b"

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
        & cover  1 (a == mempty) "a == mempty"
        & cover 50 (a /= mempty) "a /= mempty"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

makeLaw :: Testable t => String -> t -> (String, Property)
makeLaw title p = (title, checkCoverage $ property p)
