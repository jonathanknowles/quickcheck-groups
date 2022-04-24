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
import Test.Hspec
    ( Spec, describe, parallel )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Test.QuickCheck.Monoid.Subclasses
    ( cancellativeLaws
    , commutativeLaws
    , leftCancellativeLaws
    , leftReductiveLaws
    , monoidNullLaws
    , rightCancellativeLaws
    , rightReductiveLaws
    , reductiveLaws
    )

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLawsMany @[Int]
            [ leftCancellativeLaws
            , leftReductiveLaws
            , monoidNullLaws
            , rightCancellativeLaws
            , rightReductiveLaws
            ]
        testLawsMany @(Set Int)
            [ commutativeLaws
            , leftReductiveLaws
            , monoidNullLaws
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
