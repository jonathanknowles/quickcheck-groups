{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Monoid.SubclassesSpec where

import Data.Monoid
    ( Sum (..) )
import Data.Set
    ( Set )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, parallel )
import Test.QuickCheck
    ( Arbitrary (..), Gen, NonNegative (..), oneof, suchThat )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Monoid.Subclasses
    ( commutativeLaws
    , leftCancellativeLaws
    , leftReductiveLaws
    , monoidNullLaws
    , rightCancellativeLaws
    , rightReductiveLaws
    , reductiveLaws
    )

import qualified Data.Set as Set

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLawsMany @[Int]
            [ leftReductiveLaws
            , rightReductiveLaws
            , leftCancellativeLaws
            , rightCancellativeLaws
            , monoidNullLaws
            ]
        testLawsMany @(Set Int)
            [ commutativeLaws
            , leftReductiveLaws
            , rightReductiveLaws
            , reductiveLaws
            , monoidNullLaws
            ]
        testLawsMany @(Sum Int)
            [ commutativeLaws
            , leftReductiveLaws
            , rightReductiveLaws
            , reductiveLaws
            , monoidNullLaws
            ]
