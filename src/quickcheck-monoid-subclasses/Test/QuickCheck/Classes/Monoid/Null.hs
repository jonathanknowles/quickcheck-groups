{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Classes.Monoid.Null
    (
    -- * Null
      monoidNullLaws

    -- * Positive
    , positiveMonoidLaws
    )
    where

import Prelude hiding
    ( null )

import Data.Monoid.Null
    ( MonoidNull (..), PositiveMonoid )
import Data.Proxy
    ( Proxy (..) )
import Test.QuickCheck
    ( Arbitrary (..), Property )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Semigroup.Internal
    ( makeLaw1, makeLaw2, makeProperty )

--------------------------------------------------------------------------------
-- MonoidNull
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'MonoidNull'.
--
-- Tests the following property:
--
-- prop> null a == (a == mempty)
--
monoidNullLaws
    :: forall a. (Arbitrary a, Show a, Eq a, MonoidNull a)
    => Proxy a
    -> Laws
monoidNullLaws _ = Laws "MonoidNull"
    [ makeLaw1 @a
        "monoidNullLaw_basic"
        (monoidNullLaw_basic)
    ]

monoidNullLaw_basic
    :: (Eq a, MonoidNull a) => a -> Property
monoidNullLaw_basic a =
    makeProperty
        "null a == (a == mempty)"
        (null a == (a == mempty))

--------------------------------------------------------------------------------
-- PositiveMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'PositiveMonoid'.
--
-- Tests the following properties:
--
-- prop> null (a <> b) == (null a && null b)
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'monoidNullLaws'
--
positiveMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, PositiveMonoid a)
    => Proxy a
    -> Laws
positiveMonoidLaws _ = Laws "PositiveMonoid"
    [ makeLaw2 @a
        "positiveMonoidLaw_fundamental"
        (positiveMonoidLaw_fundamental)
    ]

positiveMonoidLaw_fundamental
    :: (Eq a, PositiveMonoid a) => a -> a -> Property
positiveMonoidLaw_fundamental a b =
    makeProperty
        "null (a <> b) == (null a && null b)"
        (null (a <> b) == (null a && null b))
