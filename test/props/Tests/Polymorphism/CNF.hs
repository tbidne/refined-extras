-- | Properties for CNF logic.
--
-- @since 0.1.0.0
module Tests.Polymorphism.CNF (props) where

import Gens qualified
import Hedgehog ((===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
import Refined.Extras.Polymorphism.Internal.Terms qualified as Terms
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

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
toCNFTerminates = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "toCNF converts to CNF in finite time" $
    H.withTests limit $
      H.property $ do
        calculus <- H.forAll Gens.genCalculus
        let reduced = Terms.toCNF calculus
        H.assert $ Terms.isCNF reduced

isCNFRecognizes :: TestTree
isCNFRecognizes = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "isCNF recognizes CNF formulae" $
    H.withTests limit $
      H.property $ do
        calculus <- H.forAll Gens.genCNF
        H.assert $ Terms.isCNF calculus

isCNFRejects :: TestTree
isCNFRejects = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "isCNF rejects non-CNF formulae" $
    H.withTests limit $
      H.property $ do
        calculus <- H.forAll Gens.genNonCNF
        False === Terms.isCNF calculus
