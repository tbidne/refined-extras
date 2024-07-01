-- | Properties for Text.
--
-- @since 0.1.0.0
module Tests.Predicates.Text (props) where

import Data.ByteString.Internal qualified as BSI
import Data.Char qualified as C
import Data.Either qualified as E
import Data.Proxy (Proxy)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import GHC.TypeLits (SomeSymbol (SomeSymbol))
import GHC.TypeLits qualified as TL
import Gens.Text qualified as Gens
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Refined (Predicate, RefineException, Refined)
import Refined qualified as R
import Refined.Extras.Predicates.Text
  ( Alpha,
    AlphaNum,
    Ascii,
    AsciiAlpha,
    AsciiAlphaNum,
    AsciiLower,
    AsciiUpper,
    Control,
    Digit,
    HexDigit,
    Latin1,
    Letter,
    Lower,
    Mark,
    Number,
    OctDigit,
    Punctuation,
    Separator,
    Space,
    SymEqualTo,
    Symbol,
    Upper,
  )
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Utils qualified

-- | @since 0.1.0.0
props :: TestTree
props =
  T.testGroup
    "Text properties"
    [ symbolProps,
      charUnicodeProps,
      charLatin1Props,
      word8Latin1Props
    ]

symbolProps :: TestTree
symbolProps =
  T.testGroup
    "Symbol equality"
    [ symEqualToCharSucceeds,
      symEqualToCharFails,
      symEqualToStringSucceeds,
      symEqualToStringFails,
      symEqualToTextSucceeds,
      symEqualToTextFails,
      symEqualToLazyTextSucceeds,
      symEqualToLazyTextFails
    ]

symEqualToCharSucceeds :: TestTree
symEqualToCharSucceeds =
  Utils.testPropertyCompat "Char symEqualTo succeeds" "symEqualToCharSucceeds" $
    H.property $ do
      c <- H.forAll Gens.genChar
      case TL.someSymbolVal [c] of
        SomeSymbol sym ->
          H.assert $ E.isRight $ refineFromProxy sym c

symEqualToCharFails :: TestTree
symEqualToCharFails =
  Utils.testPropertyCompat "Char symEqualTo fails" "symEqualToCharFails" $
    H.property $ do
      c <- H.forAll HG.alpha
      H.assert $ E.isLeft $ R.refine @(SymEqualTo "1") c

symEqualToStringSucceeds :: TestTree
symEqualToStringSucceeds =
  Utils.testPropertyCompat "String symEqualTo succeeds" "symEqualToStringSucceeds" $
    H.property $ do
      str <- H.forAll $ Gens.genStringX Gens.genChar
      case TL.someSymbolVal str of
        SomeSymbol sym ->
          H.assert $ E.isRight $ refineFromProxy sym str

symEqualToStringFails :: TestTree
symEqualToStringFails =
  Utils.testPropertyCompat "String symEqualTo fails" "symEqualToStringFails" $
    H.property $ do
      str <- H.forAll $ Gens.genStringX HG.alpha
      H.assert $ E.isLeft $ R.refine @(SymEqualTo "1") str

symEqualToTextSucceeds :: TestTree
symEqualToTextSucceeds =
  Utils.testPropertyCompat "Text symEqualTo succeeds" "symEqualToTextSucceeds" $
    H.property $ do
      txt <- H.forAll $ Gens.genTextX Gens.genChar
      let str = T.unpack txt
      case TL.someSymbolVal str of
        SomeSymbol sym ->
          H.assert $ E.isRight $ refineFromProxy sym txt

symEqualToTextFails :: TestTree
symEqualToTextFails =
  Utils.testPropertyCompat "Text symEqualTo fails" "symEqualToTextFails" $
    H.property $ do
      txt <- H.forAll $ Gens.genTextX HG.alpha
      H.assert $ E.isLeft $ R.refine @(SymEqualTo "1") txt

symEqualToLazyTextSucceeds :: TestTree
symEqualToLazyTextSucceeds =
  Utils.testPropertyCompat "Lazy Text symEqualTo succeeds" "symEqualToLazyTextSucceeds" $
    H.property $ do
      txt <- H.forAll $ Gens.genLazyTextX Gens.genChar
      let str = T.unpack $ LT.toStrict txt
      case TL.someSymbolVal str of
        SomeSymbol sym ->
          H.assert $ E.isRight $ refineFromProxy sym txt

symEqualToLazyTextFails :: TestTree
symEqualToLazyTextFails =
  Utils.testPropertyCompat "Lazy Text symEqualTo fails" "symEqualToLazyTextFails" $
    H.property $ do
      txt <- H.forAll $ Gens.genLazyTextX HG.alpha
      H.assert $ E.isLeft $ R.refine @(SymEqualTo "1") txt

charUnicodeProps :: TestTree
charUnicodeProps =
  Utils.testPropertyCompat "Char unicode properties" "charUnicodeProps" $
    H.property $ do
      c <- H.forAll HG.unicode
      H.diff (R.refine @Space c) matches (C.isSpace c)
      H.diff (R.refine @Lower c) matches (C.isLower c)
      H.diff (R.refine @Upper c) matches (C.isUpper c)
      H.diff (R.refine @Alpha c) matches (C.isAlpha c)
      H.diff (R.refine @AlphaNum c) matches (C.isAlphaNum c)
      H.diff (R.refine @Letter c) matches (C.isLetter c)
      H.diff (R.refine @Mark c) matches (C.isMark c)
      H.diff (R.refine @Number c) matches (C.isNumber c)
      H.diff (R.refine @Punctuation c) matches (C.isPunctuation c)
      H.diff (R.refine @Symbol c) matches (C.isSymbol c)
      H.diff (R.refine @Separator c) matches (C.isSeparator c)

charLatin1Props :: TestTree
charLatin1Props =
  Utils.testPropertyCompat "Char latin1 properties" "charLatin1Props" $
    H.property $ do
      c <- H.forAll HG.latin1
      H.diff (R.refine @Control c) matches (C.isControl c)
      H.diff (R.refine @Digit c) matches (C.isDigit c)
      H.diff (R.refine @OctDigit c) matches (C.isOctDigit c)
      H.diff (R.refine @HexDigit c) matches (C.isHexDigit c)
      H.diff (R.refine @Ascii c) matches (C.isAscii c)
      H.diff (R.refine @Latin1 c) matches (C.isLatin1 c)
      H.diff (R.refine @AsciiLower c) matches (C.isAsciiLower c)
      H.diff (R.refine @AsciiUpper c) matches (C.isAsciiUpper c)
      H.diff (R.refine @AsciiAlpha c) matches (C.isAscii c && C.isAlpha c)
      H.diff (R.refine @AsciiAlphaNum c) matches (C.isAscii c && C.isAlphaNum c)

word8Latin1Props :: TestTree
word8Latin1Props =
  Utils.testPropertyCompat "Word8 latin1 properties" "word8Latin1Props" $
    H.property $ do
      w <- H.forAll (Gens.genWord8X HG.latin1)
      let c = BSI.w2c w
      H.diff (R.refine @Control w) matches (C.isControl c)
      H.diff (R.refine @Digit w) matches (C.isDigit c)
      H.diff (R.refine @OctDigit w) matches (C.isOctDigit c)
      H.diff (R.refine @HexDigit w) matches (C.isHexDigit c)
      H.diff (R.refine @Ascii w) matches (C.isAscii c)
      H.diff (R.refine @Latin1 w) matches (C.isLatin1 c)
      H.diff (R.refine @AsciiLower w) matches (C.isAsciiLower c)
      H.diff (R.refine @AsciiUpper w) matches (C.isAsciiUpper c)
      H.diff (R.refine @AsciiAlpha w) matches (C.isAscii c && C.isAlpha c)
      H.diff (R.refine @AsciiAlphaNum w) matches (C.isAscii c && C.isAlphaNum c)

refineFromProxy ::
  forall s a.
  (Predicate (SymEqualTo s) a) =>
  Proxy s ->
  a ->
  Either RefineException (Refined (SymEqualTo s) a)
refineFromProxy _ = R.refine @(SymEqualTo s)

matches :: Either a b -> Bool -> Bool
matches (Right _) True = True
matches (Left _) False = True
matches _ _ = False
