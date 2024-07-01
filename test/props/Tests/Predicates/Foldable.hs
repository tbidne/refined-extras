-- | Properties for Foldable.
--
-- @since 0.1.0.0
module Tests.Predicates.Foldable (props) where

import Data.Either qualified as E
import Data.Proxy (Proxy (Proxy))
import Gens.Numeric qualified as Gens
import Gens.Text qualified as Gens
import Hedgehog (Gen, PropertyName)
import Hedgehog qualified as H
import Refined (EqualTo, Not, Positive, Predicate)
import Refined qualified as R
import Refined.Extras.Predicates.Foldable (All, Any, None)
import Refined.Extras.Predicates.Text (Alpha, Digit)
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Utils qualified

-- | @since 0.1.0.0
props :: TestTree
props =
  T.testGroup
    "Foldable properties"
    [ foldableProps,
      textProps,
      lazyTextProps,
      byteStringProps,
      lazyByteStringProps
    ]

-- | @since 0.1.0.0
foldableProps :: TestTree
foldableProps =
  T.testGroup
    "Foldable"
    [ allFoldableSucceeds,
      allFoldableFails,
      anyFoldableSucceeds,
      anyFoldableFails,
      noneFoldableSucceeds,
      noneFoldableFails
    ]

allFoldableSucceeds :: TestTree
allFoldableSucceeds = xSucceeds (Proxy @(All Positive)) Gens.genPositives "All succeeds" "allFoldableSucceeds"

allFoldableFails :: TestTree
allFoldableFails = xFails (Proxy @(All Positive)) Gens.genPositivesWithZero "All fails" "allFoldableFails"

anyFoldableSucceeds :: TestTree
anyFoldableSucceeds = xSucceeds (Proxy @(Any (EqualTo 0))) Gens.genPositivesWithZero "Any succeeds" "anyFoldableSucceeds"

anyFoldableFails :: TestTree
anyFoldableFails = xFails (Proxy @(Any (EqualTo 0))) Gens.genPositives "Any fails" "anyFoldableFails"

noneFoldableSucceeds :: TestTree
noneFoldableSucceeds = xSucceeds (Proxy @(None (EqualTo 0))) Gens.genPositives "None succeeds" "noneFoldableSucceeds"

noneFoldableFails :: TestTree
noneFoldableFails = xFails (Proxy @(None (EqualTo 0))) Gens.genPositivesWithZero "None fails" "noneFoldableFails"

-- | @since 0.1.0.0
textProps :: TestTree
textProps =
  T.testGroup
    "Text"
    [ allTextSucceeds,
      allTextFails,
      anyTextSucceeds,
      anyTextFails,
      noneTextSucceeds,
      noneTextFails
    ]

allTextSucceeds :: TestTree
allTextSucceeds = xSucceeds (Proxy @(All Alpha)) Gens.genTextAlpha "All succeeds" "allTextSucceeds"

allTextFails :: TestTree
allTextFails = xFails (Proxy @(All Alpha)) Gens.genTextAlphaWithDigit "All fails" "allTextFails"

anyTextSucceeds :: TestTree
anyTextSucceeds = xSucceeds (Proxy @(Any Digit)) Gens.genTextAlphaWithDigit "Any succeeds" "anyTextSucceeds"

anyTextFails :: TestTree
anyTextFails = xFails (Proxy @(Any Digit)) Gens.genTextAlpha "Any fails" "anyTextFails"

noneTextSucceeds :: TestTree
noneTextSucceeds = xSucceeds (Proxy @(None Digit)) Gens.genTextAlpha "None succeeds" "noneTextSucceeds"

noneTextFails :: TestTree
noneTextFails = xFails (Proxy @(None Digit)) Gens.genTextAlphaWithDigit "None fails" "noneTextFails"

-- | @since 0.1.0.0
lazyTextProps :: TestTree
lazyTextProps =
  T.testGroup
    "Lazy Text"
    [ allLazyTextSucceeds,
      allLazyTextFails,
      anyLazyTextSucceeds,
      anyLazyTextFails,
      noneLazyTextSucceeds,
      noneLazyTextFails
    ]

allLazyTextSucceeds :: TestTree
allLazyTextSucceeds = xSucceeds (Proxy @(All Alpha)) Gens.genLazyTextAlpha "All succeeds" "allLazyTextSucceeds"

allLazyTextFails :: TestTree
allLazyTextFails = xFails (Proxy @(All Alpha)) Gens.genLazyTextAlphaWithDigit "All fails" "allLazyTextFails"

anyLazyTextSucceeds :: TestTree
anyLazyTextSucceeds = xSucceeds (Proxy @(Any Digit)) Gens.genLazyTextAlphaWithDigit "Any succeeds" "anyLazyTextSucceeds"

anyLazyTextFails :: TestTree
anyLazyTextFails = xFails (Proxy @(Any Digit)) Gens.genLazyTextAlpha "Any fails" "anyLazyTextFails"

noneLazyTextSucceeds :: TestTree
noneLazyTextSucceeds = xSucceeds (Proxy @(None Digit)) Gens.genLazyTextAlpha "None succeeds" "noneLazyTextSucceeds"

noneLazyTextFails :: TestTree
noneLazyTextFails = xFails (Proxy @(None Digit)) Gens.genLazyTextAlphaWithDigit "None fails" "noneLazyTextFails"

-- | @since 0.1.0.0
byteStringProps :: TestTree
byteStringProps =
  T.testGroup
    "ByteString properties"
    [ allByteStringSucceeds,
      allByteStringFails,
      anyByteStringSucceeds,
      anyByteStringFails,
      noneByteStringSucceeds,
      noneByteStringFails
    ]

allByteStringSucceeds :: TestTree
allByteStringSucceeds = xSucceeds (Proxy @(All Digit)) Gens.genByteStringDigit "All succeeds" "allByteStringSucceeds"

allByteStringFails :: TestTree
allByteStringFails = xFails (Proxy @(All Digit)) Gens.genByteStringDigitWithAlpha "All fails" "allByteStringFails"

anyByteStringSucceeds :: TestTree
anyByteStringSucceeds = xSucceeds (Proxy @(Any (Not Digit))) Gens.genByteStringDigitWithAlpha "Any succeeds" "anyByteStringSucceeds"

anyByteStringFails :: TestTree
anyByteStringFails = xFails (Proxy @(Any (Not Digit))) Gens.genByteStringDigit "Any fails" "anyByteStringFails"

noneByteStringSucceeds :: TestTree
noneByteStringSucceeds = xSucceeds (Proxy @(None (Not Digit))) Gens.genByteStringDigit "None succeeds" "noneByteStringSucceeds"

noneByteStringFails :: TestTree
noneByteStringFails = xFails (Proxy @(None (Not Digit))) Gens.genByteStringDigitWithAlpha "None fails" "noneByteStringFails"

-- | @since 0.1.0.0
lazyByteStringProps :: TestTree
lazyByteStringProps =
  T.testGroup
    "Lazy ByteString properties"
    [ allLazyByteStringSucceeds,
      allLazyByteStringFails,
      anyLazyByteStringSucceeds,
      anyLazyByteStringFails,
      noneLazyByteStringSucceeds,
      noneLazyByteStringFails
    ]

allLazyByteStringSucceeds :: TestTree
allLazyByteStringSucceeds = xSucceeds (Proxy @(All Digit)) Gens.genLazyByteStringDigit "All succeeds" "allLazyByteStringSucceeds"

allLazyByteStringFails :: TestTree
allLazyByteStringFails = xFails (Proxy @(All Digit)) Gens.genLazyByteStringDigitWithAlpha "All fails" "allLazyByteStringFails"

anyLazyByteStringSucceeds :: TestTree
anyLazyByteStringSucceeds = xSucceeds (Proxy @(Any (Not Digit))) Gens.genLazyByteStringDigitWithAlpha "Any succeeds" "anyLazyByteStringSucceeds"

anyLazyByteStringFails :: TestTree
anyLazyByteStringFails = xFails (Proxy @(Any (Not Digit))) Gens.genLazyByteStringDigit "Any fails" "anyLazyByteStringFails"

noneLazyByteStringSucceeds :: TestTree
noneLazyByteStringSucceeds = xSucceeds (Proxy @(None (Not Digit))) Gens.genLazyByteStringDigit "None succeeds" "noneLazyByteStringSucceeds"

noneLazyByteStringFails :: TestTree
noneLazyByteStringFails = xFails (Proxy @(None (Not Digit))) Gens.genLazyByteStringDigitWithAlpha "None fails" "noneLazyByteStringFails"

xSucceeds ::
  forall p a.
  (Predicate p a, Show a) =>
  Proxy p ->
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
xSucceeds = xTest E.isRight

xFails ::
  forall p a.
  (Predicate p a, Show a) =>
  Proxy p ->
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
xFails = xTest E.isLeft

xTest ::
  forall p a.
  (Predicate p a, Show a) =>
  (forall e x. Either e x -> Bool) ->
  Proxy p ->
  Gen a ->
  TestName ->
  PropertyName ->
  TestTree
xTest eitherFn _ genFn desc propName =
  Utils.testPropertyCompat desc propName $
    H.property $ do
      vals <- H.forAll genFn
      H.assert $ eitherFn $ R.refine @p vals
