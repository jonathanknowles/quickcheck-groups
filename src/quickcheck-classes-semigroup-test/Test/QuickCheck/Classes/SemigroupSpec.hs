{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Classes.SemigroupSpec where

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
    ( Spec )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Classes.Semigroup
    ( cancellativeGCDMonoidLaws
    , cancellativeLaws
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
import Test.QuickCheck.Instances.ByteString
    ()
import Test.QuickCheck.Instances.Natural
    ()
import Test.QuickCheck.Instances.Text
    ()

spec :: Spec
spec = do
    testLawsMany @ByteString
        [ leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @Text
        [ leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @[Int]
        [ leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Set Int)
        [ commutativeLaws
        , gcdMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Set Natural)
        [ commutativeLaws
        , gcdMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
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
        , gcdMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
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
        [ cancellativeGCDMonoidLaws
        , cancellativeLaws
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
        ]
