-- | Exports generators.
--
-- @since 0.1.0.0
module Gens
  ( -- * Polymorphism Terms
    Prop (..),
    genCalculus,
    genCNF,
    genNonCNF,
    genImpliesA,
    genNotImpliesA,

    -- * Numeric
    genPositives,
    genPositivesWithZero,

    -- * Text
    genChar,
    genStringX,
    genTextX,

    -- ** Specific Text
    genCharControl,
    genCharSpace,
    genCharMark,
    genCharNumber,
    genCharNonAscii,
    genCharNonLatin1,
    genCharPunctuation,
    genCharSymbol,
    genCharSeparator,
  )
where

import Data.Char qualified as C
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import GHC.Natural (Natural)
import Hedgehog (GenBase, MonadGen)
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Refined.Extras.Polymorphism.Internal.Terms (Calculus (..))

data Prop = A | B | C | D
  deriving (Eq, Show)

genCalculus :: MonadGen m => m (Calculus Prop)
-- be careful with depth > 4, some of these formulae can take a long time
-- to reduce
genCalculus = genDepth 4 >>= genCalculusHelper

genCalculusHelper :: MonadGen m => Natural -> m (Calculus Prop)
genCalculusHelper 0 = genAtom
genCalculusHelper d = do
  HG.choice
    [ genAtom,
      genNot d,
      genAnd d,
      genOr d,
      genXor d
    ]

genAtom :: MonadGen m => m (Calculus Prop)
genAtom =
  HG.element
    [ CAtom A,
      CAtom B,
      CAtom C,
      CAtom D
    ]

genNot :: MonadGen f => Natural -> f (Calculus Prop)
genNot d = CNot <$> genCalculusHelper (d - 1)

genAnd :: MonadGen m => Natural -> m (Calculus Prop)
genAnd = genBinary CAnd

genOr :: MonadGen m => Natural -> m (Calculus Prop)
genOr = genBinary COr

genXor :: MonadGen m => Natural -> m (Calculus Prop)
genXor = genBinary CXor

genBinary :: MonadGen m => (Calculus Prop -> Calculus Prop -> Calculus Prop) -> Natural -> m (Calculus Prop)
genBinary _ 0 = genAtom
genBinary cons d = do
  sub1 <- genCalculusHelper (d - 1)
  sub2 <- genCalculusHelper (d - 1)
  pure $ sub1 `cons` sub2

genCNF :: MonadGen m => m (Calculus Prop)
genCNF = genDepth 5 >>= genCNFAndHelper

genCNFAndHelper :: MonadGen m => Natural -> m (Calculus Prop)
genCNFAndHelper 0 = CAnd <$> genCNFAtom <*> genCNFAtom
genCNFAndHelper d = CAnd <$> atomAnd <*> atomAnd
  where
    atomAnd = HG.choice [genCNFAndHelper (d - 1), genCNFOr, genCNFAtom]

genCNFOr :: MonadGen m => m (Calculus Prop)
genCNFOr = genDepth 5 >>= genCNFOrHelper

genCNFOrHelper :: MonadGen m => Natural -> m (Calculus Prop)
genCNFOrHelper 0 = COr <$> genCNFAtom <*> genCNFAtom
genCNFOrHelper d = COr <$> atomOr <*> atomOr
  where
    atomOr = HG.choice [genCNFOrHelper (d - 1), genCNFAtom]

genCNFAtom :: MonadGen m => m (Calculus Prop)
genCNFAtom = HG.choice [CNot <$> genAtom, genAtom]

genNonCNF :: MonadGen m => m (Calculus Prop)
genNonCNF =
  HG.choice
    [ genNonCNFNot,
      genNonCNFOr,
      genXor 1
    ]

genNonCNFOr :: MonadGen m => m (Calculus Prop)
genNonCNFOr =
  HG.choice
    [ COr <$> genAnd 1 <*> genAtom,
      COr <$> genAtom <*> genAnd 1
    ]

genNonCNFNot :: MonadGen m => m (Calculus Prop)
genNonCNFNot =
  HG.choice
    [ CNot . CNot <$> genAtom,
      CNot <$> genOr 1,
      CNot <$> genAnd 1,
      CNot <$> genXor 1
    ]

genDepth :: MonadGen m => Int -> m Natural
genDepth upperBound = fromIntegral <$> HG.int (HR.constant 0 upperBound)

genImpliesA :: MonadGen m => m (Calculus Prop)
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

genNotImpliesA :: MonadGen m => m (Calculus Prop)
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

genPositives :: MonadGen m => m [Natural]
genPositives = genWithLowerBound 1

genPositivesWithZero :: MonadGen m => m [Natural]
genPositivesWithZero = do
  positives <- genWithLowerBound 1
  HG.shuffle $ 0 : positives

genWithLowerBound :: MonadGen m => Int -> m [Natural]
genWithLowerBound lower = HG.list listSz genPositive
  where
    genPositive = fromIntegral <$> HG.int (HR.exponential lower 10_000)
    listSz = HR.constant 0 20

genChar :: MonadGen m => m Char
genChar = HG.unicodeAll

genCharControl :: (MonadGen m, GenBase m ~ Identity) => m Char
genCharControl = HG.filter C.isControl HG.latin1

genCharSpace :: MonadGen m => m Char
genCharSpace =
  HG.element $
    C.chr
      <$> [ -- Unicode Space Separator
            0x0020,
            0x00A0,
            0x1680,
            0x2000,
            0x2001,
            0x2002,
            0x2003,
            0x2004,
            0x2005,
            0x2006,
            0x2007,
            0x2008,
            0x2009,
            0x200A,
            0x202F,
            0x205F,
            0x3000
          ]

genCharNumber :: forall m. MonadGen m => m Char
genCharNumber =
  HG.choice
    [ HG.digit,
      genLetterNumber,
      genOtherNumber
    ]
  where
    genLetterNumber =
      HG.element $
        C.chr
          <$> [ 0x16EE,
                0x2177,
                0x3029,
                0x1015A,
                0x1246E
              ]
    genOtherNumber =
      HG.element $
        C.chr
          <$> [ 0x00B2,
                0x0F2F,
                0x2078,
                0x10E70,
                0x1F10C
              ]

genCharNonAscii :: (GenBase m ~ Identity, MonadGen m) => m Char
genCharNonAscii = HG.filter (not . C.isAscii) HG.unicodeAll

genCharNonLatin1 :: (GenBase m ~ Identity, MonadGen m) => m Char
genCharNonLatin1 = HG.filter (not . C.isLatin1) HG.unicodeAll

-- There are thousands of these, so only going to list a few.
genCharMark :: MonadGen m => m Char
genCharMark =
  HG.element $
    C.chr
      <$> [ -- Spacing mark
            0x0903,
            0x09C0,
            0x0B57,
            0x0D4A,
            0x11935,
            0x1D172,
            -- Enclosing mark
            0x0488,
            0x1ABE,
            0x20DD,
            0x20E4,
            0xA672,
            -- Non-Spacing mark
            0x0300,
            0x0319,
            0x07F3,
            0x0818,
            0x1A65
          ]

genCharPunctuation :: MonadGen m => m Char
genCharPunctuation =
  HG.element $
    C.chr
      <$> [ -- Unicode Connector Punctuation
            0x005F,
            0xFF3F,
            -- Unicode Dash Punctuation
            0x002D,
            0x10EAD,
            -- Unicode Close Punctuation
            0x0029,
            0xFF63,
            -- Unicode Final Punctuation
            0x00BB,
            0x2E21,
            -- Unicode Initial Punctuation
            0x00AB,
            0x2E20,
            -- Unicode Other Punctuation
            0x0021,
            0x1E95F,
            -- Unicode Open Punctuation
            0x0028,
            0xFF62
          ]

genCharSymbol :: MonadGen m => m Char
genCharSymbol =
  HG.element $
    C.chr
      <$> [ -- Unicode Currency Symbol
            0x0024,
            0x1EcB0,
            -- Unicode Modifier Symbol
            0x005E,
            0x1F3FF,
            -- Unicode Math Symbol
            0x002B,
            0x1EEF1,
            -- Unicode Other Symbol
            0x00A6,
            0x1FBCA
          ]

genCharSeparator :: MonadGen m => m Char
genCharSeparator = HG.choice [genCharSpace, otherSep]
  where
    otherSep =
      HG.element $
        C.chr
          <$> [ -- Unicode Line Separator
                0x2028,
                -- Unicode Paragraph Separator
                0x2029
              ]

genStringX :: MonadGen m => m Char -> m String
genStringX = HG.string maxStrSz

genTextX :: MonadGen m => m Char -> m Text
genTextX = HG.text maxStrSz

maxStrSz :: Integral a => HR.Range a
maxStrSz = HR.exponential 0 100
