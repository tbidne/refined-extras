-- | Properties for implication logic.
--
-- @since 0.1.0.0
module Tests.Polymorphism.Implies (props) where

import Gens.Polymorphism (Prop (A))
import Gens.Polymorphism qualified as Gens
import Hedgehog ((===))
import Hedgehog qualified as H
import Refined.Extras.Polymorphism.Internal.Terms (Calculus (CAtom))
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
impliesSucceeds =
  Utils.testPropertyCompat "impliesBool recognizes proof" "impliesSucceeds" $
    H.property $ do
      calculus <- H.forAll Gens.genImpliesA
      H.assert $ Terms.impliesBool calculus (CAtom A)

impliesFails :: TestTree
impliesFails =
  Utils.testPropertyCompat "impliesBool rejects proof" "impliesFails" $
    H.property $ do
      calculus <- H.forAll Gens.genNotImpliesA
      False === Terms.impliesBool calculus (CAtom A)
