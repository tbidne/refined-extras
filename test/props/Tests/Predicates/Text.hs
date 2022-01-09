-- | Properties for Text.
--
-- @since 0.1.0.0
module Tests.Predicates.Text (props) where

import Data.Either qualified as E
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import GHC.TypeLits (SomeSymbol (..))
import GHC.TypeLits qualified as TL
import Gens qualified
import Hedgehog (Gen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import MaxRuns (MaxRuns (..))
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
import Test.Tasty (TestName, TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | @since 0.1.0.0
props :: TestTree
props =
  T.testGroup
    "Text properties"
    [ symbolProps,
      unicodeProps,
      asciiProps
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
symEqualToCharSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Char symEqualTo succeeds" $
    H.withTests limit $
      H.property $ do
        c <- H.forAll Gens.genChar
        case TL.someSymbolVal [c] of
          SomeSymbol sym ->
            H.assert $ E.isRight $ refineFromProxy sym c

symEqualToCharFails :: TestTree
symEqualToCharFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Char symEqualTo fails" $
    H.withTests limit $
      H.property $ do
        c <- H.forAll HG.alpha
        H.assert $ E.isLeft $ R.refine @(SymEqualTo "1") c

symEqualToStringSucceeds :: TestTree
symEqualToStringSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "String symEqualTo succeeds" $
    H.withTests limit $
      H.property $ do
        str <- H.forAll $ Gens.genStringX Gens.genChar
        case TL.someSymbolVal str of
          SomeSymbol sym ->
            H.assert $ E.isRight $ refineFromProxy sym str

symEqualToStringFails :: TestTree
symEqualToStringFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "String symEqualTo fails" $
    H.withTests limit $
      H.property $ do
        str <- H.forAll $ Gens.genStringX HG.alpha
        H.assert $ E.isLeft $ R.refine @(SymEqualTo "1") str

symEqualToTextSucceeds :: TestTree
symEqualToTextSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Text symEqualTo succeeds" $
    H.withTests limit $
      H.property $ do
        txt <- H.forAll $ Gens.genTextX Gens.genChar
        let str = T.unpack txt
        case TL.someSymbolVal str of
          SomeSymbol sym ->
            H.assert $ E.isRight $ refineFromProxy sym txt

symEqualToTextFails :: TestTree
symEqualToTextFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Text symEqualTo fails" $
    H.withTests limit $
      H.property $ do
        txt <- H.forAll $ Gens.genTextX HG.alpha
        H.assert $ E.isLeft $ R.refine @(SymEqualTo "1") txt

symEqualToLazyTextSucceeds :: TestTree
symEqualToLazyTextSucceeds = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Lazy Text symEqualTo succeeds" $
    H.withTests limit $
      H.property $ do
        txt <- H.forAll $ Gens.genLazyTextX Gens.genChar
        let str = T.unpack $ LT.toStrict txt
        case TL.someSymbolVal str of
          SomeSymbol sym ->
            H.assert $ E.isRight $ refineFromProxy sym txt

symEqualToLazyTextFails :: TestTree
symEqualToLazyTextFails = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Lazy Text symEqualTo fails" $
    H.withTests limit $
      H.property $ do
        txt <- H.forAll $ Gens.genLazyTextX HG.alpha
        H.assert $ E.isLeft $ R.refine @(SymEqualTo "1") txt

unicodeProps :: TestTree
unicodeProps =
  T.testGroup
    "Unicode predicates"
    [ spaceCharSucceeds,
      spaceCharFails,
      lowerCharSucceeds,
      lowerCharFails,
      upperCharSucceeds,
      upperCharFails,
      alphaCharSucceeds,
      alphaCharFails,
      alphaNumCharSucceeds,
      alphaNumCharFails,
      letterCharSucceeds,
      letterCharFails,
      markCharSucceeds,
      markCharFails,
      numberCharSucceeds,
      numberCharFails,
      punctuationCharSucceeds,
      punctuationCharFails,
      symbolCharSucceeds,
      symbolCharFails,
      separatorCharSucceeds,
      separatorCharFails
    ]

spaceCharSucceeds :: TestTree
spaceCharSucceeds = xSucceeds (Proxy @Space) "Space Char succeeds" Gens.genCharSpace

spaceCharFails :: TestTree
spaceCharFails = xFails (Proxy @Space) "Space Char fails" HG.digit

lowerCharSucceeds :: TestTree
lowerCharSucceeds = xSucceeds (Proxy @Lower) "Lower Char succeeds" HG.lower

lowerCharFails :: TestTree
lowerCharFails = xFails (Proxy @Lower) "Lower Char fails" HG.upper

upperCharSucceeds :: TestTree
upperCharSucceeds = xSucceeds (Proxy @Upper) "Upper Char succeeds" HG.upper

upperCharFails :: TestTree
upperCharFails = xFails (Proxy @Upper) "Upper Char fails" HG.lower

alphaCharSucceeds :: TestTree
alphaCharSucceeds = xSucceeds (Proxy @Alpha) "Alpha Char succeeds" HG.alpha

alphaCharFails :: TestTree
alphaCharFails = xFails (Proxy @Alpha) "Alpha Char fails" HG.digit

alphaNumCharSucceeds :: TestTree
alphaNumCharSucceeds = xSucceeds (Proxy @AlphaNum) "AlphaNum Char succeeds" HG.alphaNum

alphaNumCharFails :: TestTree
alphaNumCharFails = xFails (Proxy @AlphaNum) "AlphaNum Char fails" Gens.genCharSpace

letterCharSucceeds :: TestTree
letterCharSucceeds = xSucceeds (Proxy @Letter) "Letter Char succeeds" HG.alpha

letterCharFails :: TestTree
letterCharFails = xFails (Proxy @Letter) "Letter Char fails" HG.digit

markCharSucceeds :: TestTree
markCharSucceeds = xSucceeds (Proxy @Mark) "Mark Char succeeds" Gens.genCharMark

markCharFails :: TestTree
markCharFails = xFails (Proxy @Mark) "Mark Char fails" HG.digit

numberCharSucceeds :: TestTree
numberCharSucceeds = xSucceeds (Proxy @Number) "Number Char succeeds" Gens.genCharNumber

numberCharFails :: TestTree
numberCharFails = xFails (Proxy @Number) "Number Char fails" HG.alpha

punctuationCharSucceeds :: TestTree
punctuationCharSucceeds = xSucceeds (Proxy @Punctuation) "Punctuation Char succeeds" Gens.genCharPunctuation

punctuationCharFails :: TestTree
punctuationCharFails = xFails (Proxy @Punctuation) "Punctuation Char fails" HG.digit

symbolCharSucceeds :: TestTree
symbolCharSucceeds = xSucceeds (Proxy @Symbol) "Symbol Char succeeds" Gens.genCharSymbol

symbolCharFails :: TestTree
symbolCharFails = xFails (Proxy @Symbol) "Symbol Char fails" HG.digit

separatorCharSucceeds :: TestTree
separatorCharSucceeds = xSucceeds (Proxy @Separator) "Separator Char succeeds" Gens.genCharSeparator

separatorCharFails :: TestTree
separatorCharFails = xFails (Proxy @Separator) "Separator Char fails" HG.digit

asciiProps :: TestTree
asciiProps =
  T.testGroup
    "Ascii/Latin1 predicates"
    [ controlCharSucceeds,
      controlCharFails,
      controlWord8Succeeds,
      controlWord8Fails,
      digitCharSucceeds,
      digitCharFails,
      digitWord8Succeeds,
      digitWord8Fails,
      octalCharSucceeds,
      octalCharFails,
      octalWord8Succeeds,
      octalWord8Fails,
      hexCharSucceeds,
      hexCharFails,
      hexWord8Succeeds,
      hexWord8Fails,
      asciiCharSucceeds,
      asciiCharFails,
      asciiWord8Succeeds,
      asciiWord8Fails,
      latin1CharSucceeds,
      latin1CharFails,
      latin1Word8Succeeds,
      asciiLowerCharSucceeds,
      asciiLowerCharFails,
      asciiLowerWord8Succeeds,
      asciiLowerWord8Fails,
      asciiUpperCharSucceeds,
      asciiUpperCharFails,
      asciiUpperWord8Succeeds,
      asciiUpperWord8Fails,
      asciiAlphaCharSucceeds,
      asciiAlphaCharFails,
      asciiAlphaWord8Succeeds,
      asciiAlphaWord8Fails,
      asciiAlphaNumCharSucceeds,
      asciiAlphaNumCharFails,
      asciiAlphaNumWord8Succeeds,
      asciiAlphaNumWord8Fails
    ]

controlCharSucceeds :: TestTree
controlCharSucceeds = xSucceeds (Proxy @Control) "Control Char succeeds" Gens.genCharControl

controlCharFails :: TestTree
controlCharFails = xFails (Proxy @Control) "Control Char fails" HG.digit

controlWord8Succeeds :: TestTree
controlWord8Succeeds = xSucceeds (Proxy @Control) "Control Word8 succeeds" (Gens.genWord8X Gens.genCharControl)

controlWord8Fails :: TestTree
controlWord8Fails = xFails (Proxy @Control) "Control Word8 fails" (Gens.genWord8X HG.digit)

digitCharSucceeds :: TestTree
digitCharSucceeds = xSucceeds (Proxy @Digit) "Digit Char succeeds" HG.digit

digitCharFails :: TestTree
digitCharFails = xFails (Proxy @Digit) "Digit Char fails" HG.alpha

digitWord8Succeeds :: TestTree
digitWord8Succeeds = xSucceeds (Proxy @Digit) "Digit Word8 succeeds" (Gens.genWord8X HG.digit)

digitWord8Fails :: TestTree
digitWord8Fails = xFails (Proxy @Digit) "Digit Word8 fails" (Gens.genWord8X HG.alpha)

octalCharSucceeds :: TestTree
octalCharSucceeds = xSucceeds (Proxy @OctDigit) "Octal Char succeeds" HG.octit

octalCharFails :: TestTree
octalCharFails = xFails (Proxy @OctDigit) "Octal Char fails" HG.alpha

octalWord8Succeeds :: TestTree
octalWord8Succeeds = xSucceeds (Proxy @OctDigit) "Octal Word8 succeeds" (Gens.genWord8X HG.octit)

octalWord8Fails :: TestTree
octalWord8Fails = xFails (Proxy @OctDigit) "Octal Word8 fails" (Gens.genWord8X HG.alpha)

hexCharSucceeds :: TestTree
hexCharSucceeds = xSucceeds (Proxy @HexDigit) "Hex Char succeeds" HG.hexit

hexCharFails :: TestTree
hexCharFails = xFails (Proxy @HexDigit) "Hex Char fails" Gens.genCharSpace

hexWord8Succeeds :: TestTree
hexWord8Succeeds = xSucceeds (Proxy @HexDigit) "Hex Word8 succeeds" (Gens.genWord8X HG.hexit)

hexWord8Fails :: TestTree
hexWord8Fails = xFails (Proxy @HexDigit) "Hex Word8 fails" (Gens.genWord8X Gens.genCharSpace)

asciiCharSucceeds :: TestTree
asciiCharSucceeds = xSucceeds (Proxy @Ascii) "Ascii Char succeeds" HG.ascii

asciiCharFails :: TestTree
asciiCharFails = xFails (Proxy @Ascii) "Ascii Char fails" Gens.genCharNonAscii

asciiWord8Succeeds :: TestTree
asciiWord8Succeeds = xSucceeds (Proxy @Ascii) "Ascii Word8 succeeds" HG.ascii

asciiWord8Fails :: TestTree
asciiWord8Fails = xFails (Proxy @Ascii) "Ascii Word8 fails" (Gens.genWord8X Gens.genCharLatin1NoAscii)

latin1CharSucceeds :: TestTree
latin1CharSucceeds = xSucceeds (Proxy @Latin1) "Latin1 Char succeeds" HG.latin1

latin1CharFails :: TestTree
latin1CharFails = xFails (Proxy @Latin1) "Latin1 Char fails" Gens.genCharNonLatin1

latin1Word8Succeeds :: TestTree
latin1Word8Succeeds = xSucceeds (Proxy @Latin1) "Latin1 Word8 succeeds" (Gens.genWord8X HG.latin1)

-- No latin1Word8Fails because Word8 is trivially Latin1, i.e., should always
-- succeed

asciiLowerCharSucceeds :: TestTree
asciiLowerCharSucceeds = xSucceeds (Proxy @AsciiLower) "AsciiLower Char succeeds" HG.lower

asciiLowerCharFails :: TestTree
asciiLowerCharFails = xFails (Proxy @AsciiLower) "AsciiLower Char fails" HG.upper

asciiLowerWord8Succeeds :: TestTree
asciiLowerWord8Succeeds = xSucceeds (Proxy @AsciiLower) "AsciiLower Word8 succeeds" (Gens.genWord8X HG.lower)

asciiLowerWord8Fails :: TestTree
asciiLowerWord8Fails = xFails (Proxy @AsciiLower) "AsciiLower Word8 fails" (Gens.genWord8X HG.upper)

asciiUpperCharSucceeds :: TestTree
asciiUpperCharSucceeds = xSucceeds (Proxy @AsciiUpper) "AsciiUpper Char succeeds" HG.upper

asciiUpperCharFails :: TestTree
asciiUpperCharFails = xFails (Proxy @AsciiUpper) "AsciiUpper Char fails" HG.lower

asciiUpperWord8Succeeds :: TestTree
asciiUpperWord8Succeeds = xSucceeds (Proxy @AsciiUpper) "AsciiUpper Word8 succeeds" (Gens.genWord8X HG.upper)

asciiUpperWord8Fails :: TestTree
asciiUpperWord8Fails = xFails (Proxy @AsciiUpper) "AsciiUpper Word8 fails" (Gens.genWord8X HG.lower)

asciiAlphaCharSucceeds :: TestTree
asciiAlphaCharSucceeds = xSucceeds (Proxy @AsciiAlpha) "AsciiAlpha Char succeeds" HG.alpha

asciiAlphaCharFails :: TestTree
asciiAlphaCharFails = xFails (Proxy @AsciiAlpha) "AsciiAlpha Char fails" HG.digit

asciiAlphaWord8Succeeds :: TestTree
asciiAlphaWord8Succeeds = xSucceeds (Proxy @AsciiAlpha) "AsciiAlpha Word8 succeeds" (Gens.genWord8X HG.alpha)

asciiAlphaWord8Fails :: TestTree
asciiAlphaWord8Fails = xFails (Proxy @AsciiAlpha) "AsciiAlpha Word8 fails" (Gens.genWord8X HG.digit)

asciiAlphaNumCharSucceeds :: TestTree
asciiAlphaNumCharSucceeds = xSucceeds (Proxy @AsciiAlphaNum) "AsciiAlphaNum Char succeeds" HG.alphaNum

asciiAlphaNumCharFails :: TestTree
asciiAlphaNumCharFails = xFails (Proxy @AsciiAlphaNum) "AsciiAlphaNum Char fails" Gens.genCharSpace

asciiAlphaNumWord8Succeeds :: TestTree
asciiAlphaNumWord8Succeeds = xSucceeds (Proxy @AsciiAlphaNum) "AsciiAlphaNum Word8 succeeds" (Gens.genWord8X HG.alphaNum)

asciiAlphaNumWord8Fails :: TestTree
asciiAlphaNumWord8Fails = xFails (Proxy @AsciiAlphaNum) "AsciiAlphaNum Word8 fails" (Gens.genWord8X Gens.genCharSpace)

xSucceeds ::
  forall p a.
  (Predicate p a, Show a) =>
  Proxy p ->
  TestName ->
  Gen a ->
  TestTree
xSucceeds = xTest E.isRight

xFails ::
  forall p a.
  (Predicate p a, Show a) =>
  Proxy p ->
  TestName ->
  Gen a ->
  TestTree
xFails = xTest E.isLeft

xTest ::
  forall p a.
  (Predicate p a, Show a) =>
  (forall e x. Either e x -> Bool) ->
  Proxy p ->
  TestName ->
  Gen a ->
  TestTree
xTest testEither _ desc gen = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty desc $
    H.withTests limit $
      H.property $ do
        c <- H.forAll gen
        H.assert $ testEither $ R.refine @p c

refineFromProxy ::
  forall s a.
  (Predicate (SymEqualTo s) a) =>
  Proxy s ->
  a ->
  Either RefineException (Refined (SymEqualTo s) a)
refineFromProxy _ = R.refine @(SymEqualTo s)
