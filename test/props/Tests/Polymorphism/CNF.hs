-- | Properties for CNF logic.
--
-- @since 0.1.0.0
module Tests.Polymorphism.CNF (props) where

import Gens.Polymorphism qualified as Gens
import Hedgehog ((===))
import Hedgehog qualified as H
import Refined.Extras.Polymorphism.Internal.Terms qualified as Terms
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Utils qualified

-- | @since 0.1.0.0
props :: TestTree
props =
  T.testGroup
    "CNF properties"
    [ toCNFTerminates,
      isCNFRecognizes,
      isCNFRejects
    ]

toCNFTerminates :: TestTree
toCNFTerminates =
  Utils.testPropertyCompat "toCNF converts to CNF in finite time" "toCNFTerminates" $
    H.property $ do
      calculus <- H.forAll Gens.genCalculus
      let reduced = Terms.toCNF calculus
      H.assert $ Terms.isCNF reduced

isCNFRecognizes :: TestTree
isCNFRecognizes =
  Utils.testPropertyCompat "isCNF recognizes CNF formulae" "isCNFRecognizes" $
    H.property $ do
      calculus <- H.forAll Gens.genCNF
      H.assert $ Terms.isCNF calculus

isCNFRejects :: TestTree
isCNFRejects =
  Utils.testPropertyCompat "isCNF rejects non-CNF formulae" "isCNFRejects" $
    H.property $ do
      calculus <- H.forAll Gens.genNonCNF
      False === Terms.isCNF calculus
