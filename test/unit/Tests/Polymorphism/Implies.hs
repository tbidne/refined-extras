{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Unit tests for the Implies, ImpliesBool type families.
--
-- @since 0.1.0.0
module Tests.Polymorphism.Implies (tests) where

import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import Refined
  ( Negative,
    NonZero,
    Not,
    Positive,
    Refined,
    Xor,
    type (&&),
    type (||),
  )
import Refined qualified as R
import Refined.Extras.Polymorphism (Implies, ImpliesBool)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

-- | @since 0.1.0.0
tests :: TestTree
tests =
  Tasty.testGroup
    "Implies tests"
    [ impliesTests,
      impliesBoolTests
    ]

impliesTests :: TestTree
impliesTests =
  Tasty.testGroup
    "Implies Constraint tests"
    [ impliesSimple,
      impliesAnd,
      impliesOr,
      impliesXor,
      impliesNot
    ]

impliesSimple :: TestTree
impliesSimple = THU.testCase "A => A" $ do
  () @=? requiresNonZero $$(R.refineTH @NonZero @Int 1)

impliesAnd :: TestTree
impliesAnd = THU.testCase "A && B => A" $ do
  () @=? requiresNonZero $$(R.refineTH @(NonZero && Positive) @Int 1)

impliesOr :: TestTree
impliesOr = THU.testCase "A || A => A" $ do
  () @=? requiresNonZero $$(R.refineTH @(NonZero || NonZero) @Int 1)

impliesXor :: TestTree
impliesXor = THU.testCase "A Xor (A && B) => A" $ do
  () @=? requiresNonZero $$(R.refineTH @(NonZero `Xor` (NonZero && Negative)) @Int 1)

impliesNot :: TestTree
impliesNot = THU.testCase "Not (Not A) ==> A" $ do
  () @=? requiresNonZero $$(R.refineTH @(Not (Not NonZero)) @Int 1)

impliesBoolTests :: TestTree
impliesBoolTests =
  Tasty.testGroup
    "ImpliesBool tests"
    [ impliesBoolSimpleTrue,
      impliesBoolAndTrue,
      impliesBoolOrTrue,
      impliesBoolXorTrue,
      impliesBoolNotTrue,
      impliesBoolSimpleFalse,
      impliesBoolAndFalse,
      impliesBoolOrFalse,
      impliesBoolXorFalse,
      impliesBoolNotFalse
    ]

impliesBoolSimpleTrue :: TestTree
impliesBoolSimpleTrue = THU.testCase "A => A" $ do
  True @=? demote (Proxy @(ImpliesBool NonZero NonZero))

impliesBoolAndTrue :: TestTree
impliesBoolAndTrue = THU.testCase "A && B => A" $ do
  True @=? demote (Proxy @(ImpliesBool (NonZero && Positive) NonZero))

impliesBoolAndTrue2 :: TestTree
impliesBoolAndTrue2 = THU.testCase "A && B => A && B" $ do
  True @=? demote (Proxy @(ImpliesBool (NonZero && Positive) (NonZero && Positive)))

impliesBoolAndTrue3 :: TestTree
impliesBoolAndTrue3 = THU.testCase "A && B => B && A" $ do
  True @=? demote (Proxy @(ImpliesBool (NonZero && Positive) (Positive && NonZero)))

impliesBoolOrTrue :: TestTree
impliesBoolOrTrue = THU.testCase "A || B => A" $ do
  True @=? demote (Proxy @(ImpliesBool (NonZero || NonZero) NonZero))

impliesBoolOrTrue2 :: TestTree
impliesBoolOrTrue2 = THU.testCase "A || B => A || B" $ do
  True @=? demote (Proxy @(ImpliesBool (NonZero || Positive) (NonZero || Positive)))

impliesBoolOrTrue3 :: TestTree
impliesBoolOrTrue3 = THU.testCase "A || B => B || A" $ do
  True @=? demote (Proxy @(ImpliesBool (NonZero || Positive) (Positive || NonZero)))

impliesBoolXorTrue :: TestTree
impliesBoolXorTrue = THU.testCase "A Xor A => A" $ do
  True @=? demote (Proxy @(ImpliesBool (NonZero `Xor` NonZero) NonZero))

impliesBoolXorTrue2 :: TestTree
impliesBoolXorTrue2 = THU.testCase "A Xor B => A Xor B" $ do
  True @=? demote (Proxy @(ImpliesBool (NonZero `Xor` Positive) (Positive `Xor` NonZero)))

impliesBoolXorTrue3 :: TestTree
impliesBoolXorTrue3 = THU.testCase "A Xor B => B Xor A" $ do
  True @=? demote (Proxy @(ImpliesBool (NonZero `Xor` Positive) (Positive `Xor` NonZero)))

impliesBoolNotTrue :: TestTree
impliesBoolNotTrue = THU.testCase "Not (Not A) ==> A" $ do
  True @=? demote (Proxy @(ImpliesBool (Not (Not NonZero)) NonZero))

impliesBoolSimpleFalse :: TestTree
impliesBoolSimpleFalse = THU.testCase "A /=> B" $ do
  False @=? demote (Proxy @(ImpliesBool Positive Negative))

impliesBoolAndFalse :: TestTree
impliesBoolAndFalse = THU.testCase "A && B /=> C" $ do
  False @=? demote (Proxy @(ImpliesBool (NonZero && Positive) Negative))

impliesBoolOrFalse :: TestTree
impliesBoolOrFalse = THU.testCase "A || B /=> A" $ do
  False @=? demote (Proxy @(ImpliesBool (NonZero || Positive) NonZero))

impliesBoolXorFalse :: TestTree
impliesBoolXorFalse = THU.testCase "A Xor B /=> A" $ do
  False @=? demote (Proxy @(ImpliesBool (NonZero `Xor` Positive) NonZero))

impliesBoolNotFalse :: TestTree
impliesBoolNotFalse = THU.testCase "Not A /=> A" $ do
  False @=? demote (Proxy @(ImpliesBool (Not NonZero) NonZero))

requiresNonZero :: Implies p NonZero => Refined p Int -> ()
requiresNonZero _ = ()

type Demote :: forall k. k -> Constraint
class Demote a where
  demote :: Proxy a -> Bool

instance Demote 'True where demote _ = True

instance Demote 'False where demote _ = False