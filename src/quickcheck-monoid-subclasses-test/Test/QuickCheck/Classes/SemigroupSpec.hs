{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Classes.SemigroupSpec where

import Data.ByteString.Lazy
    ( ByteString )
import Data.Map.Strict
    ( Map )
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
import Test.QuickCheck
    ( Property )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Classes.Semigroup
    ( cancellativeGCDMonoidLaws
    , gcdMonoidLaws
    , groupLaws
    , leftGCDMonoidLaws
    , monoidNullLaws
    , monusLaws
    , overlappingGCDMonoidLaws
    , positiveMonoidLaws
    , rightGCDMonoidLaws
    )
import Test.QuickCheck.Classes.Semigroup.Cancellative
    ( cancellativeLaws
    , commutativeLaws
    , leftCancellativeLaws
    , leftReductiveLaws
    , reductiveLaws
    , rightCancellativeLaws
    , rightReductiveLaws
    )
import Test.QuickCheck.Instances.ByteString
    ()
import Test.QuickCheck.Instances.Natural
    ()
import Test.QuickCheck.Instances.Text
    ()
import Test.QuickCheck.Property
    ( Result (..), mapTotalResult )

spec :: Spec
spec = do
    testLawsMany @() $ disableCoverageCheck
        [ cancellativeGCDMonoidLaws
        , cancellativeLaws
        , commutativeLaws
        , gcdMonoidLaws
        , groupLaws
        , leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @ByteString
        [ leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
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
        , positiveMonoidLaws
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
        , positiveMonoidLaws
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
        , positiveMonoidLaws
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
        , positiveMonoidLaws
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
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Sum Int)
        [ cancellativeLaws
        , commutativeLaws
        , groupLaws
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
        , positiveMonoidLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Map Int Int)
        [ leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        -- TODO:
        -- Determine why these laws are not satisfied:
        -- overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Map Int Natural)
        [ leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        -- TODO:
        -- Determine why these laws are not satisfied:
        -- overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightReductiveLaws
        ]

--------------------------------------------------------------------------------
-- Coverage checks
--------------------------------------------------------------------------------

class HasCoverageCheck p where
    disableCoverageCheck :: p -> p

instance HasCoverageCheck Laws where
    disableCoverageCheck (Laws title laws) =
        Laws title $ fmap disableCoverageCheck <$> laws

instance HasCoverageCheck Property where
    disableCoverageCheck =
        mapTotalResult (\r -> r {maybeCheckCoverage = Nothing})

instance (Functor f, HasCoverageCheck p) => HasCoverageCheck (f p) where
    disableCoverageCheck =
        fmap disableCoverageCheck
