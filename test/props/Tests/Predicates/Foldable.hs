-- | Properties for Foldable.
--
-- @since 0.1.0.0
module Tests.Predicates.Foldable (props) where

import Data.Either qualified as E
import Data.Proxy (Proxy (..))
import Gens.Numeric qualified as Gens
import Gens.Text qualified as Gens
import Hedgehog (Gen)
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Refined (EqualTo, Not, Positive, Predicate)
import Refined qualified as R
import Refined.Extras.Predicates.Foldable (All, Any, None)
import Refined.Extras.Predicates.Text (Alpha, Digit)
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

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
allFoldableSucceeds = xSucceeds (Proxy @(All Positive)) Gens.genPositives "All succeeds"

allFoldableFails :: TestTree
allFoldableFails = xFails (Proxy @(All Positive)) Gens.genPositivesWithZero "All fails"

anyFoldableSucceeds :: TestTree
anyFoldableSucceeds = xSucceeds (Proxy @(Any (EqualTo 0))) Gens.genPositivesWithZero "Any succeeds"

anyFoldableFails :: TestTree
anyFoldableFails = xFails (Proxy @(Any (EqualTo 0))) Gens.genPositives "Any fails"

noneFoldableSucceeds :: TestTree
noneFoldableSucceeds = xSucceeds (Proxy @(None (EqualTo 0))) Gens.genPositives "None succeeds"

noneFoldableFails :: TestTree
noneFoldableFails = xFails (Proxy @(None (EqualTo 0))) Gens.genPositivesWithZero "None fails"

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
allTextSucceeds = xSucceeds (Proxy @(All Alpha)) Gens.genTextAlpha "All succeeds"

allTextFails :: TestTree
allTextFails = xFails (Proxy @(All Alpha)) Gens.genTextAlphaWithDigit "All fails"

anyTextSucceeds :: TestTree
anyTextSucceeds = xSucceeds (Proxy @(Any Digit)) Gens.genTextAlphaWithDigit "Any succeeds"

anyTextFails :: TestTree
anyTextFails = xFails (Proxy @(Any Digit)) Gens.genTextAlpha "Any fails"

noneTextSucceeds :: TestTree
noneTextSucceeds = xSucceeds (Proxy @(None Digit)) Gens.genTextAlpha "None succeeds"

noneTextFails :: TestTree
noneTextFails = xFails (Proxy @(None Digit)) Gens.genTextAlphaWithDigit "None fails"

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
allLazyTextSucceeds = xSucceeds (Proxy @(All Alpha)) Gens.genLazyTextAlpha "All succeeds"

allLazyTextFails :: TestTree
allLazyTextFails = xFails (Proxy @(All Alpha)) Gens.genLazyTextAlphaWithDigit "All fails"

anyLazyTextSucceeds :: TestTree
anyLazyTextSucceeds = xSucceeds (Proxy @(Any Digit)) Gens.genLazyTextAlphaWithDigit "Any succeeds"

anyLazyTextFails :: TestTree
anyLazyTextFails = xFails (Proxy @(Any Digit)) Gens.genLazyTextAlpha "Any fails"

noneLazyTextSucceeds :: TestTree
noneLazyTextSucceeds = xSucceeds (Proxy @(None Digit)) Gens.genLazyTextAlpha "None succeeds"

noneLazyTextFails :: TestTree
noneLazyTextFails = xFails (Proxy @(None Digit)) Gens.genLazyTextAlphaWithDigit "None fails"

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
allByteStringSucceeds = xSucceeds (Proxy @(All Digit)) Gens.genByteStringDigit "All succeeds"

allByteStringFails :: TestTree
allByteStringFails = xFails (Proxy @(All Digit)) Gens.genByteStringDigitWithAlpha "All fails"

anyByteStringSucceeds :: TestTree
anyByteStringSucceeds = xSucceeds (Proxy @(Any (Not Digit))) Gens.genByteStringDigitWithAlpha "Any succeeds"

anyByteStringFails :: TestTree
anyByteStringFails = xFails (Proxy @(Any (Not Digit))) Gens.genByteStringDigit "Any fails"

noneByteStringSucceeds :: TestTree
noneByteStringSucceeds = xSucceeds (Proxy @(None (Not Digit))) Gens.genByteStringDigit "None succeeds"

noneByteStringFails :: TestTree
noneByteStringFails = xFails (Proxy @(None (Not Digit))) Gens.genByteStringDigitWithAlpha "None fails"

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
allLazyByteStringSucceeds = xSucceeds (Proxy @(All Digit)) Gens.genLazyByteStringDigit "All succeeds"

allLazyByteStringFails :: TestTree
allLazyByteStringFails = xFails (Proxy @(All Digit)) Gens.genLazyByteStringDigitWithAlpha "All fails"

anyLazyByteStringSucceeds :: TestTree
anyLazyByteStringSucceeds = xSucceeds (Proxy @(Any (Not Digit))) Gens.genLazyByteStringDigitWithAlpha "Any succeeds"

anyLazyByteStringFails :: TestTree
anyLazyByteStringFails = xFails (Proxy @(Any (Not Digit))) Gens.genLazyByteStringDigit "Any fails"

noneLazyByteStringSucceeds :: TestTree
noneLazyByteStringSucceeds = xSucceeds (Proxy @(None (Not Digit))) Gens.genLazyByteStringDigit "None succeeds"

noneLazyByteStringFails :: TestTree
noneLazyByteStringFails = xFails (Proxy @(None (Not Digit))) Gens.genLazyByteStringDigitWithAlpha "None fails"

xSucceeds ::
  forall p a.
  (Predicate p a, Show a) =>
  Proxy p ->
  Gen a ->
  TestName ->
  TestTree
xSucceeds = xTest E.isRight

xFails ::
  forall p a.
  (Predicate p a, Show a) =>
  Proxy p ->
  Gen a ->
  TestName ->
  TestTree
xFails = xTest E.isLeft

xTest ::
  forall p a.
  (Predicate p a, Show a) =>
  (forall e x. Either e x -> Bool) ->
  Proxy p ->
  Gen a ->
  TestName ->
  TestTree
xTest eitherFn _ genFn desc = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty desc $
    H.withTests limit $
      H.property $ do
        vals <- H.forAll genFn
        H.assert $ eitherFn $ R.refine @p vals
