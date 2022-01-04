-- | Unit tests for the CNF type families.
--
-- @since 0.1.0.0
module Tests.Polymorphism.CNF (tests) where

import Data.Proxy (Proxy (..))
import Refined
  ( Not,
    Xor,
    type (&&),
    type (||),
  )
import Refined.Extras.Polymorphism.Internal (IsCNF, ToCNF)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as THU

data A

data B

data C

data D

-- | @since 0.1.0.0
tests :: TestTree
tests =
  Tasty.testGroup
    "CNF tests"
    [ toCNFTests,
      cnfRecognizesTests,
      cnfRejectsTests
    ]

toCNFTests :: TestTree
toCNFTests =
  Tasty.testGroup
    "ToCNF tests"
    [ toCNF,
      toCNF2,
      toCNFXor,
      toCNFNot,
      toCNFDeMorgan,
      toCNFDeMorgan2,
      toCNFDist,
      toCNFDist2
    ]

toCNF :: TestTree
toCNF = THU.testCase "A -> A" $ do
  Proxy @A @=? Proxy @(ToCNF A)

toCNF2 :: TestTree
toCNF2 = THU.testCase "A || (B && (Not C || D)) -> (A || B) && (A || Not C || D)" $ do
  Proxy @((A || B) && (A || Not C || D)) @=? Proxy @(ToCNF (A || (B && (Not C || D))))

toCNFXor :: TestTree
toCNFXor = THU.testCase "A Xor B -> (A || B) && (Not A || Not B)" $ do
  Proxy @((A || B) && (Not A || Not B)) @=? Proxy @(ToCNF (A `Xor` B))

toCNFNot :: TestTree
toCNFNot = THU.testCase "Not (Not A) -> A" $ do
  Proxy @A @=? Proxy @(ToCNF (Not (Not A)))

toCNFDeMorgan :: TestTree
toCNFDeMorgan = THU.testCase "Not (A || B) -> Not A && Not B" $ do
  Proxy @(Not A && Not B) @=? Proxy @(ToCNF (Not (A || B)))

toCNFDeMorgan2 :: TestTree
toCNFDeMorgan2 = THU.testCase "Not (A && B) -> Not A || Not B" $ do
  Proxy @(Not A || Not B) @=? Proxy @(ToCNF (Not (A && B)))

toCNFDist :: TestTree
toCNFDist = THU.testCase "A || (B && C) -> (A || B) && (A || C)" $ do
  Proxy @((A || B) && (A || C)) @=? Proxy @(ToCNF (A || (B && C)))

toCNFDist2 :: TestTree
toCNFDist2 = THU.testCase "A || (B && C) -> (A || B) && (A || C)" $ do
  Proxy @((A || C) && (B || C)) @=? Proxy @(ToCNF ((A && B) || C))

cnfRecognizesTests :: TestTree
cnfRecognizesTests =
  Tasty.testGroup
    "CNF recognizes tests"
    [ isCNFSimpleRecognizes,
      isCNFAndRecognizes,
      isCNFAndRecognizes2,
      isCNFAndRecognizes3,
      isCNFOrRecognizes,
      isCNFOrRecognizes2,
      isCNFOrRecognizes3,
      isCNFNotRecognizes
    ]

isCNFSimpleRecognizes :: TestTree
isCNFSimpleRecognizes = THU.testCase "A" $ do
  Proxy @'True @=? Proxy @(IsCNF A)

isCNFAndRecognizes :: TestTree
isCNFAndRecognizes = THU.testCase "A && B" $ do
  Proxy @'True @=? Proxy @(IsCNF (A && B))

isCNFAndRecognizes2 :: TestTree
isCNFAndRecognizes2 = THU.testCase "A && B && C" $ do
  Proxy @'True @=? Proxy @(IsCNF (A && B && C))

isCNFAndRecognizes3 :: TestTree
isCNFAndRecognizes3 = THU.testCase "A && (B || Not C || A) && Not C" $ do
  Proxy @'True @=? Proxy @(IsCNF (A && (B || Not C || A) && Not C))

isCNFOrRecognizes :: TestTree
isCNFOrRecognizes = THU.testCase "A || B is CNF" $ do
  Proxy @'True @=? Proxy @(IsCNF (A || B))

isCNFOrRecognizes2 :: TestTree
isCNFOrRecognizes2 = THU.testCase "A || B || C" $ do
  Proxy @'True @=? Proxy @(IsCNF (A || B || C))

isCNFOrRecognizes3 :: TestTree
isCNFOrRecognizes3 = THU.testCase "A || Not B || C" $ do
  Proxy @'True @=? Proxy @(IsCNF (A || Not B || C))

isCNFNotRecognizes :: TestTree
isCNFNotRecognizes = THU.testCase "A || Not B || C" $ do
  Proxy @'True @=? Proxy @(IsCNF (Not A))

cnfRejectsTests :: TestTree
cnfRejectsTests =
  Tasty.testGroup
    "CNF rejects tests"
    [ isCNFAndRejects,
      isCNFAndRejects2,
      isCNFOrRejects,
      isCNFOrRejects2,
      isCNFOrRejects3,
      isCNFXorRejects,
      isCNFNotRejects,
      isCNFNotRejects2,
      isCNFNotRejects3,
      isCNFNotRejects4
    ]

isCNFAndRejects :: TestTree
isCNFAndRejects = THU.testCase "A && (Not (Not B) && C)" $ do
  Proxy @'False @=? Proxy @(IsCNF (A && (Not (Not B) || C)))

isCNFAndRejects2 :: TestTree
isCNFAndRejects2 = THU.testCase "A && (B || (C && D))" $ do
  Proxy @'False @=? Proxy @(IsCNF (A && (B || (C && D))))

isCNFOrRejects :: TestTree
isCNFOrRejects = THU.testCase "A || (B && C)" $ do
  Proxy @'False @=? Proxy @(IsCNF (A || (B && C)))

isCNFOrRejects2 :: TestTree
isCNFOrRejects2 = THU.testCase "(A && B) || C" $ do
  Proxy @'False @=? Proxy @(IsCNF ((A && B) || C))

isCNFOrRejects3 :: TestTree
isCNFOrRejects3 = THU.testCase "A || Not (Not B)" $ do
  Proxy @'False @=? Proxy @(IsCNF (A || Not (Not A)))

isCNFXorRejects :: TestTree
isCNFXorRejects = THU.testCase "A Xor B" $ do
  Proxy @'False @=? Proxy @(IsCNF (A `Xor` B))

isCNFNotRejects :: TestTree
isCNFNotRejects = THU.testCase "Not (Not A)" $ do
  Proxy @'False @=? Proxy @(IsCNF (Not (Not A)))

isCNFNotRejects2 :: TestTree
isCNFNotRejects2 = THU.testCase "Not (A && B)" $ do
  Proxy @'False @=? Proxy @(IsCNF (Not (A && B)))

isCNFNotRejects3 :: TestTree
isCNFNotRejects3 = THU.testCase "Not (A || B)" $ do
  Proxy @'False @=? Proxy @(IsCNF (Not (A || B)))

isCNFNotRejects4 :: TestTree
isCNFNotRejects4 = THU.testCase "Not (A Xor B)" $ do
  Proxy @'False @=? Proxy @(IsCNF (Not (A `Xor` B)))
