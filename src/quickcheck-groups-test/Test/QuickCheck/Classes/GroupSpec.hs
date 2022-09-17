{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Classes.GroupSpec where

import Data.Bifunctor
    ( bimap )
import Data.Monoid
    ( Product (..), Sum (..) )
import Data.Ratio
    ( denominator, numerator, (%) )
import Test.Hspec
    ( Spec )
import Test.QuickCheck
    ( Arbitrary (..), NonZero (..), Property, choose, oneof )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Group
    ( groupLaws )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
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
        [groupLaws]
    testLawsMany @(Product TestRational)
        [groupLaws]
    testLawsMany @(Sum TestRational)
        [groupLaws]

--------------------------------------------------------------------------------
-- Test types
--------------------------------------------------------------------------------

newtype TestRational = TestRational Rational
    deriving stock (Eq, Show)
    deriving newtype (Num, Fractional)

instance Arbitrary TestRational where
    arbitrary =
        fmap TestRational $ (%) <$> genSmall <*> genSmall
      where
        genSmall = oneof [choose (-4, -1), choose (1, 4)]
    shrink (TestRational r) =
        TestRational . uncurry (%) . bimap getNonZero getNonZero
            <$> shrink (NonZero (numerator r), NonZero (denominator r))

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
