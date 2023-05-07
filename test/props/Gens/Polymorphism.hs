-- | Exports generators for polymorphism.
--
-- @since 0.1.0.0
module Gens.Polymorphism
  ( Prop (..),
    genCalculus,
    genCNF,
    genNonCNF,
    genImpliesA,
    genNotImpliesA,
  )
where

import GHC.Natural (Natural)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Refined.Extras.Polymorphism.Internal.Terms (Calculus (..))

data Prop = A | B | C | D
  deriving stock (Eq, Show)

genCalculus :: (MonadGen m) => m (Calculus Prop)
-- be careful with depth > 4, some of these formulae can take a long time
-- to reduce
genCalculus = genDepth 4 >>= genCalculusHelper

genCalculusHelper :: (MonadGen m) => Natural -> m (Calculus Prop)
genCalculusHelper 0 = genAtom
genCalculusHelper d = do
  HG.choice
    [ genAtom,
      genNot d,
      genAnd d,
      genOr d,
      genXor d
    ]

genAtom :: (MonadGen m) => m (Calculus Prop)
genAtom =
  HG.element
    [ CAtom A,
      CAtom B,
      CAtom C,
      CAtom D
    ]

genNot :: (MonadGen f) => Natural -> f (Calculus Prop)
genNot d = CNot <$> genCalculusHelper (d - 1)

genAnd :: (MonadGen m) => Natural -> m (Calculus Prop)
genAnd = genBinary CAnd

genOr :: (MonadGen m) => Natural -> m (Calculus Prop)
genOr = genBinary COr

genXor :: (MonadGen m) => Natural -> m (Calculus Prop)
genXor = genBinary CXor

genBinary :: (MonadGen m) => (Calculus Prop -> Calculus Prop -> Calculus Prop) -> Natural -> m (Calculus Prop)
genBinary _ 0 = genAtom
genBinary cons d = do
  sub1 <- genCalculusHelper (d - 1)
  sub2 <- genCalculusHelper (d - 1)
  pure $ sub1 `cons` sub2

genCNF :: (MonadGen m) => m (Calculus Prop)
genCNF = genDepth 5 >>= genCNFAndHelper

genCNFAndHelper :: (MonadGen m) => Natural -> m (Calculus Prop)
genCNFAndHelper 0 = CAnd <$> genCNFAtom <*> genCNFAtom
genCNFAndHelper d = CAnd <$> atomAnd <*> atomAnd
  where
    atomAnd = HG.choice [genCNFAndHelper (d - 1), genCNFOr, genCNFAtom]

genCNFOr :: (MonadGen m) => m (Calculus Prop)
genCNFOr = genDepth 5 >>= genCNFOrHelper

genCNFOrHelper :: (MonadGen m) => Natural -> m (Calculus Prop)
genCNFOrHelper 0 = COr <$> genCNFAtom <*> genCNFAtom
genCNFOrHelper d = COr <$> atomOr <*> atomOr
  where
    atomOr = HG.choice [genCNFOrHelper (d - 1), genCNFAtom]

genCNFAtom :: (MonadGen m) => m (Calculus Prop)
genCNFAtom = HG.choice [CNot <$> genAtom, genAtom]

genNonCNF :: (MonadGen m) => m (Calculus Prop)
genNonCNF =
  HG.choice
    [ genNonCNFNot,
      genNonCNFOr,
      genXor 1
    ]

genNonCNFOr :: (MonadGen m) => m (Calculus Prop)
genNonCNFOr =
  HG.choice
    [ COr <$> genAnd 1 <*> genAtom,
      COr <$> genAtom <*> genAnd 1
    ]

genNonCNFNot :: (MonadGen m) => m (Calculus Prop)
genNonCNFNot =
  HG.choice
    [ CNot . CNot <$> genAtom,
      CNot <$> genOr 1,
      CNot <$> genAnd 1,
      CNot <$> genXor 1
    ]

genDepth :: (MonadGen m) => Int -> m Natural
genDepth upperBound = fromIntegral <$> HG.int (HR.constant 0 upperBound)

genImpliesA :: (MonadGen m) => m (Calculus Prop)
genImpliesA = do
  HG.choice
    [ genA,
      CNot . CNot <$> genA,
      CAnd <$> genA <*> genAtom,
      CAnd <$> genAtom <*> genA,
      COr <$> genA <*> genA,
      CXor <$> genA <*> genA
    ]
  where
    genA = pure $ CAtom A

genNotImpliesA :: (MonadGen m) => m (Calculus Prop)
genNotImpliesA = do
  HG.choice
    [ genNonA,
      CNot <$> genA,
      CAnd <$> genNonA <*> genNonA,
      COr <$> genA <*> genNonA,
      COr <$> genNonA <*> genA,
      CXor <$> genA <*> genNonA
    ]
  where
    genA = pure $ CAtom A
    genNonA =
      HG.element
        [ CAtom B,
          CAtom C,
          CAtom D
        ]
