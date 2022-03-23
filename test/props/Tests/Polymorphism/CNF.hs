-- | Properties for CNF logic.
--
-- @since 0.1.0.0
module Tests.Polymorphism.CNF (props) where

import Gens.Polymorphism qualified as Gens
import Hedgehog ((===))
import Hedgehog qualified as H
import MaxRuns (MaxRuns (..))
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
toCNFTerminates = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "toCNF converts to CNF in finite time" "toCNFTerminates" $
    H.withTests limit $
      H.property $ do
        calculus <- H.forAll Gens.genCalculus
        let reduced = Terms.toCNF calculus
        H.assert $ Terms.isCNF reduced

isCNFRecognizes :: TestTree
isCNFRecognizes = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "isCNF recognizes CNF formulae" "isCNFRecognizes" $
    H.withTests limit $
      H.property $ do
        calculus <- H.forAll Gens.genCNF
        H.assert $ Terms.isCNF calculus

isCNFRejects :: TestTree
isCNFRejects = T.askOption $ \(MkMaxRuns limit) ->
  Utils.testPropertyCompat "isCNF rejects non-CNF formulae" "isCNFRejects" $
    H.withTests limit $
      H.property $ do
        calculus <- H.forAll Gens.genNonCNF
        False === Terms.isCNF calculus
