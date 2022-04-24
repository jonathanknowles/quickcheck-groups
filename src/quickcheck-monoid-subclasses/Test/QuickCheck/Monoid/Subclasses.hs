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
    ( cancellativeLaws
    , commutativeLaws
    , leftCancellativeLaws
    , leftReductiveLaws
    , monoidNullLaws
    , reductiveLaws
    , rightCancellativeLaws
    , rightReductiveLaws
    )
    where

import Prelude hiding
    ( null )

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
    [ makeLaw2 @a "Basic"
        commutativeLaw_basic
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
    [ makeLaw2 @a "isPrefix"
        leftReductiveLaw_isPrefix
    , makeLaw2 @a "isPrefix stripPrefix"
        leftReductiveLaw_isPrefix_stripPrefix
    , makeLaw2 @a "isPrefix stripPrefix (Just)"
        leftReductiveLaw_isPrefix_stripPrefix_Just
    , makeLaw2 @a "stripPrefix"
        leftReductiveLaw_stripPrefix
    , makeLaw2 @a "stripPrefix (Just)"
        leftReductiveLaw_stripPrefix_Just
    ]

leftReductiveLaw_isPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix a b = property $
    a `isPrefixOf` (a <> b)

leftReductiveLaw_isPrefix_stripPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix_stripPrefix a b = property $
    isPrefixOf a b == isJust (stripPrefix a b)

leftReductiveLaw_isPrefix_stripPrefix_Just
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_isPrefix_stripPrefix_Just a b =
    isJust (stripPrefix a (a <> b))
        ==> leftReductiveLaw_isPrefix_stripPrefix a (a <> b)

leftReductiveLaw_stripPrefix
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix a b = property $
    maybe b (a <>) (stripPrefix a b) == b

leftReductiveLaw_stripPrefix_Just
    :: (Eq a, LeftReductive a) => a -> a -> Property
leftReductiveLaw_stripPrefix_Just a b =
    isJust (stripPrefix a (a <> b))
        ==> leftReductiveLaw_stripPrefix a (a <> b)

--------------------------------------------------------------------------------
-- RightReductive
--------------------------------------------------------------------------------

rightReductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightReductive a)
    => Proxy a
    -> Laws
rightReductiveLaws _ = Laws "RightReductive"
    [ makeLaw2 @a "isSuffix"
        rightReductiveLaw_isSuffix
    , makeLaw2 @a "isSuffix stripSuffix"
        rightReductiveLaw_isSuffix_stripSuffix
    , makeLaw2 @a "isSuffix stripSuffix (Just)"
        rightReductiveLaw_isSuffix_stripSuffix_Just
    , makeLaw2 @a "stripSuffix"
        rightReductiveLaw_stripSuffix
    , makeLaw2 @a "stripSuffix (Just)"
        rightReductiveLaw_stripSuffix_Just
    ]

rightReductiveLaw_isSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix a b = property $
    b `isSuffixOf` (a <> b)

rightReductiveLaw_isSuffix_stripSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix_stripSuffix a b = property $
    isSuffixOf a b == isJust (stripSuffix a b)

rightReductiveLaw_isSuffix_stripSuffix_Just
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_isSuffix_stripSuffix_Just a b =
    isJust (stripSuffix b (a <> b))
    ==> rightReductiveLaw_isSuffix_stripSuffix b (a <> b)

rightReductiveLaw_stripSuffix
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix a b = property $
    maybe b (<> a) (stripSuffix a b) == b

rightReductiveLaw_stripSuffix_Just
    :: (Eq a, RightReductive a) => a -> a -> Property
rightReductiveLaw_stripSuffix_Just a b =
    isJust (stripSuffix b (a <> b))
    ==> rightReductiveLaw_stripSuffix a (a <> b)

--------------------------------------------------------------------------------
-- Reductive
--------------------------------------------------------------------------------

reductiveLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Reductive a)
    => Proxy a
    -> Laws
reductiveLaws _ = Laws "Reductive"
    [ makeLaw2 @a "Equivalence (prefix)"
        reductiveLaw_equivalence_prefix
    , makeLaw2 @a "Equivalence (prefix) (Just)"
        reductiveLaw_equivalence_prefix_Just
    , makeLaw2 @a "Equivalence (suffix)"
        reductiveLaw_equivalence_suffix
    , makeLaw2 @a "Equivalence (suffix) (Just)"
        reductiveLaw_equivalence_suffix_Just
    , makeLaw2 @a "Inversion (prefix)"
        reductiveLaw_inversion_prefix
    , makeLaw2 @a "Inversion (prefix) (Just)"
        reductiveLaw_inversion_prefix_Just
    , makeLaw2 @a "Inversion (suffix)"
        reductiveLaw_inversion_suffix
    , makeLaw2 @a "Inversion (suffix) (Just)"
        reductiveLaw_inversion_suffix_Just
    ]

reductiveLaw_equivalence_prefix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_prefix a b = property $
    a </> b == stripPrefix b a

reductiveLaw_equivalence_prefix_Just
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_prefix_Just a b =
    isJust (stripPrefix a (a <> b))
    ==> reductiveLaw_equivalence_prefix a (a <> b)

reductiveLaw_equivalence_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix a b = property $
    a </> b == stripSuffix b a

reductiveLaw_equivalence_suffix_Just
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_equivalence_suffix_Just a b =
    isJust (stripSuffix b (a <> b))
    ==> reductiveLaw_equivalence_suffix b (a <> b)

reductiveLaw_inversion_prefix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix a b = property $
    maybe a (b <>) (a </> b) == a

reductiveLaw_inversion_prefix_Just
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_prefix_Just a b =
    isJust ((a <> b) </> a)
    ==> reductiveLaw_inversion_prefix (a <> b) a

reductiveLaw_inversion_suffix
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix a b = property $
    maybe a (<> b) (a </> b) == a

reductiveLaw_inversion_suffix_Just
    :: (Eq a, Reductive a) => a -> a -> Property
reductiveLaw_inversion_suffix_Just a b =
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
    [ makeLaw2 @a "Cancellation"
        leftCancellativeLaw_cancellation
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
    [ makeLaw2 @a "Cancellation"
        rightCancellativeLaw_cancellation
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
    [ makeLaw2 @a "Cancellation (prefix)"
        cancellativeLaw_cancellation_prefix
    , makeLaw2 @a "Cancellation (suffix)"
        cancellativeLaw_cancellation_suffix
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
-- MonoidNull
--------------------------------------------------------------------------------

monoidNullLaws
    :: forall a. (Arbitrary a, Show a, Eq a, MonoidNull a)
    => Proxy a
    -> Laws
monoidNullLaws _ = Laws "MonoidNull"
    [ makeLaw1 @a "Basic"
        monoidNullLaw_basic
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
makeLaw1 s p = makeLaw s $ makeProperty1 p

makeLaw2
    :: (Arbitrary a, Show a, Eq a, Testable t)
    => String
    -> (a -> a -> t)
    -> (String, Property)
makeLaw2 s p = makeLaw s $ makeProperty2 p

makeProperty1 :: (Eq a, Monoid a, Testable t) => (a -> t) -> (a -> Property)
makeProperty1 p a
    = cover  1 (a == mempty) "a == mempty"
    $ cover 50 (a /= mempty) "a /= mempty"
    $ property $ p a

makeProperty2 :: (Eq a, Testable t) => (a -> a -> t) -> (a -> a -> Property)
makeProperty2 p a b
    = cover  1 (a == b) "a == b"
    $ cover 50 (a /= b) "a /= b"
    $ property $ p a b
