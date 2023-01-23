-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Classes.Group.Internal
    ( makeLaw0
    , makeLaw1
    , makeLaw2
    , makeLaw3
    , makeProperty
    )
    where

import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Eq
    ( allUnique, canVerifyAllNonNull )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Testable
    , checkCoverage
    , counterexample
    , cover
    , property
    )
import Test.QuickCheck.Classes.Group.Tuple
    ( Tuple1, Tuple2, Tuple3, evalTuple1, evalTuple2, evalTuple3 )

makeLaw :: Testable t => String -> t -> (String, Property)
makeLaw title t = (title, checkCoverage $ property t)

makeLaw0
    :: forall a. (Eq a, Monoid a)
    => String
    -> (Proxy a -> Property)
    -> (String, Property)
makeLaw0 s = makeLaw s . makeProperty0

makeLaw1
    :: (Arbitrary a, Show a, Eq a, Monoid a, Testable t)
    => String
    -> (a -> t)
    -> (String, Property)
makeLaw1 s = makeLaw s . makeProperty1

makeLaw2
    :: (Arbitrary a, Show a, Eq a, Semigroup a, Testable t)
    => String
    -> (a -> a -> t)
    -> (String, Property)
makeLaw2 s = makeLaw s . makeProperty2

makeLaw3
    :: (Arbitrary a, Show a, Eq a, Semigroup a, Testable t)
    => String
    -> (a -> a -> a -> t)
    -> (String, Property)
makeLaw3 s = makeLaw s . makeProperty3

makeProperty :: Testable t => String -> t -> Property
makeProperty propertyDescription t =
    property t & counterexample counterexampleText
  where
    counterexampleText = unlines
        [ "Property not satisfied:"
        , propertyDescription
            & fmap replaceSpecialChars
        ]
      where
        replaceSpecialChars = \case
            'λ'   -> '\\'
            other -> other

makeProperty0
    :: forall a t. Testable t
    => (Proxy a -> t)
    -> Property
makeProperty0 p = property $ p $ Proxy @a

makeProperty1
    :: (Eq a, Monoid a, Testable t)
    => (a -> t)
    -> (Tuple1 a -> Property)
makeProperty1 p (evalTuple1 -> a)
    = cover 1 (a == mempty) "a == mempty"
    $ cover 1 (a /= mempty) "a /= mempty"
    $ property $ p a

makeProperty2
    :: (Eq a, Semigroup a, Testable t)
    => (a -> a -> t)
    -> (Tuple2 a -> Property)
makeProperty2 p (evalTuple2 -> (a, b))
    = cover 1
        (allUnique [a, b])
        "allUnique [a, b]"
    $ cover 1
        (canVerifyAllNonNull [a, b])
        "canVerifyAllNonNull [a, b]"
    $ cover 1
        (allUnique [a, b] && canVerifyAllNonNull [a, b])
        "allUnique [a, b] && canVerifyAllNonNull [a, b]"
    $ property $ p a b

makeProperty3
    :: (Eq a, Semigroup a, Testable t)
    => (a -> a -> a -> t)
    -> (Tuple3 a -> Property)
makeProperty3 p (evalTuple3 -> (a, b, c))
    = cover 1
        (allUnique [a, b, c])
        "allUnique [a, b, c]"
    $ cover 1
        (canVerifyAllNonNull [a, b, c])
        "canVerifyAllNonNull [a, b, c]"
    $ cover 1
        (allUnique [a, b, c] && canVerifyAllNonNull [a, b, c])
        "allUnique [a, b, c] && canVerifyAllNonNull [a, b, c]"
    $ property $ p a b c
