{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Monoid.SubclassesSpec where

import Data.ByteString.Lazy
    ( ByteString )
import Data.Monoid
    ( Product (..), Sum (..) )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, parallel )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Instances.ByteString
    ()
import Test.QuickCheck.Instances.Natural
    ()
import Test.QuickCheck.Instances.Text
    ()
import Test.QuickCheck.Monoid.Subclasses
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

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLawsMany @ByteString
            [ leftCancellativeLaws
            , leftReductiveLaws
            , monoidNullLaws
            , overlappingGCDMonoidLaws
            , rightCancellativeLaws
            , rightReductiveLaws
            ]
        testLawsMany @Text
            [ leftCancellativeLaws
            , leftReductiveLaws
            , monoidNullLaws
            , overlappingGCDMonoidLaws
            , rightCancellativeLaws
            , rightReductiveLaws
            ]
        testLawsMany @[Int]
            [ leftCancellativeLaws
            , leftReductiveLaws
            , monoidNullLaws
            , overlappingGCDMonoidLaws
            , rightCancellativeLaws
            , rightReductiveLaws
            ]
        testLawsMany @(Set Int)
            [ commutativeLaws
            , leftReductiveLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , reductiveLaws
            , rightReductiveLaws
            ]
        testLawsMany @(Set Natural)
            [ commutativeLaws
            , leftReductiveLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , reductiveLaws
            , rightReductiveLaws
            ]
        testLawsMany @(Product Int)
            [ commutativeLaws
            , leftReductiveLaws
            , monoidNullLaws
            , reductiveLaws
            , rightReductiveLaws
            ]
        testLawsMany @(Product Natural)
            [ commutativeLaws
            , leftReductiveLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , reductiveLaws
            , rightReductiveLaws
            ]
        testLawsMany @(Sum Int)
            [ cancellativeLaws
            , commutativeLaws
            , leftCancellativeLaws
            , leftReductiveLaws
            , monoidNullLaws
            , reductiveLaws
            , rightCancellativeLaws
            , rightReductiveLaws
            ]
        testLawsMany @(Sum Natural)
            [ cancellativeLaws
            , commutativeLaws
            , leftCancellativeLaws
            , leftReductiveLaws
            , monoidNullLaws
            , monusLaws
            , overlappingGCDMonoidLaws
            , reductiveLaws
            , rightCancellativeLaws
            , rightReductiveLaws
            ]
