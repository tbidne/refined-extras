-- | Properties for Foldable.
--
-- @since 0.1.0.0
module Tests.Predicates.Foldable (props) where

import Data.Either qualified as E
import Gens qualified
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Refined (EqualTo, Positive)
import Refined qualified as R
import Refined.Extras.Predicates.Foldable (All, Any, None)
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | @since 0.1.0.0
props :: TestTree
props =
  T.testGroup
    "Foldable properties"
    [ allSucceeds,
      allFails,
      anySucceeds,
      anyFails,
      noneSucceeds,
      noneFails
    ]

allSucceeds :: TestTree
allSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "All (> 0) succeeds" $
    H.withTests limit $
      H.property $ do
        positives <- H.forAll Gens.genPositives
        H.assert $ E.isRight $ R.refine @(All Positive) positives

allFails :: TestTree
allFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "All (> 0) fails" $
    H.withTests limit $
      H.property $ do
        positives <- H.forAll Gens.genPositivesWithZero
        H.assert $ E.isLeft $ R.refine @(All Positive) positives

anySucceeds :: TestTree
anySucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Any (== 0) succeeds" $
    H.withTests limit $
      H.property $ do
        positives <- H.forAll Gens.genPositivesWithZero
        H.assert $ E.isRight $ R.refine @(Any (EqualTo 0)) positives

anyFails :: TestTree
anyFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Any (== 0) fails" $
    H.withTests limit $
      H.property $ do
        positives <- H.forAll Gens.genPositives
        H.assert $ E.isLeft $ R.refine @(Any (EqualTo 0)) positives

noneSucceeds :: TestTree
noneSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "None (== 0) succeeds" $
    H.withTests limit $
      H.property $ do
        positives <- H.forAll Gens.genPositives
        H.assert $ E.isRight $ R.refine @(None (EqualTo 0)) positives

noneFails :: TestTree
noneFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "None (== 0) succeeds" $
    H.withTests limit $
      H.property $ do
        positives <- H.forAll Gens.genPositivesWithZero
        H.assert $ E.isLeft $ R.refine @(None (EqualTo 0)) positives
