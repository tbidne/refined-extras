-- | Properties for implication logic.
--
-- @since 0.1.0.0
module Tests.Polymorphism.Implies (props) where

import Gens.Polymorphism (Prop (..))
import Gens.Polymorphism qualified as Gens
import Hedgehog ((===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Refined.Extras.Polymorphism.Internal.Terms (Calculus (..))
import Refined.Extras.Polymorphism.Internal.Terms qualified as Terms
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Utils qualified

-- | @since 0.1.0.0
props :: TestTree
props =
  T.testGroup
    "Implies properties"
    [ impliesSucceeds,
      impliesFails
    ]

impliesSucceeds :: TestTree
impliesSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "impliesBool recognizes proof" "impliesSucceeds" $
    H.withTests limit $
      H.property $ do
        calculus <- H.forAll Gens.genImpliesA
        H.assert $ Terms.impliesBool calculus (CAtom A)

impliesFails :: TestTree
impliesFails = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "impliesBool rejects proof" "impliesFails" $
    H.withTests limit $
      H.property $ do
        calculus <- H.forAll Gens.genNotImpliesA
        False === Terms.impliesBool calculus (CAtom A)
