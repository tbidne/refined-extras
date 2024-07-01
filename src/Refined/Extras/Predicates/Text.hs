-- | Predicates for 'Text' and 'String'.
--
-- @since 0.1.0.0
module Refined.Extras.Predicates.Text
  ( -- * Symbol Equality
    SymEqualTo,

    -- * Char Predicates
    -- $char

    -- ** Unicode
    -- $unicode
    Space,
    Lower,
    Upper,
    Alpha,
    AlphaNum,
    Letter,
    Mark,
    Number,
    Punctuation,
    Symbol,
    Separator,

    -- ** Ascii/Latin1
    -- $ascii
    Control,
    Digit,
    OctDigit,
    HexDigit,
    Ascii,
    Latin1,
    AsciiUpper,
    AsciiLower,
    AsciiAlpha,
    AsciiAlphaNum,
  )
where

import Data.ByteString.Internal qualified as BS
import Data.Char qualified as C
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Typeable qualified as Ty
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol)
import GHC.TypeLits qualified as TL
import Refined (Predicate (validate), RefineException (RefineOtherException))

-- $setup
-- >>> import Data.ByteString qualified as BS
-- >>> import Data.Either (isRight)
-- >>> import Refined (refine)
-- >>> import Refined.Extras.Predicates.Foldable (All)
-- >>> import Refined.Extras.Utils (showRefineException)
-- >>> import Data.Text.Encoding (encodeUtf8)
-- >>> let ch = C.chr 0x0266
-- >>> :{
--   instance Predicate Alpha Word8 where
--     validate proxy w
--       | C.isAlpha c = Nothing
--       | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
--       where
--         c = BS.w2c w
--         err = T.singleton c <> " is not an alphabetic character"
-- :}

-- | Predicate equality for symbols.
--
-- ==== __Examples__
-- >>> validate @(SymEqualTo "c") Proxy 'c'
-- Nothing
--
-- >>> showRefineException <$> validate @(SymEqualTo "abc") Proxy 'c'
-- Just "RefineOtherException (SymEqualTo \"abc\") \"c is not a single Char\""
--
-- >>> validate @(SymEqualTo "abc") Proxy "abc"
-- Nothing
--
-- >>> showRefineException <$> validate @(SymEqualTo "123") @Text Proxy "abc"
-- Just "RefineOtherException (SymEqualTo \"123\") \"abc does not equal the predicate\""
--
-- @since 0.1.0.0
type SymEqualTo :: TL.Symbol -> Type
data SymEqualTo c
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance (KnownSymbol c) => Predicate (SymEqualTo c) Char where
  validate proxy x = case sym of
    [y] ->
      if x == y
        then Nothing
        else Just $ RefineOtherException (Ty.typeRep proxy) eqErr
    _ -> Just $ RefineOtherException (Ty.typeRep proxy) nonCharErr
    where
      sym = TL.symbolVal @c Proxy
      eqErr = T.singleton x <> " does not equal the predicate"
      nonCharErr = T.singleton x <> " is not a single Char"

-- | @since 0.1.0.0
instance (KnownSymbol c) => Predicate (SymEqualTo c) String where
  validate proxy txt
    | txt == sym = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      sym = TL.symbolVal @c Proxy
      err = T.pack $ txt <> " does not equal the predicate"

-- | @since 0.1.0.0
instance (KnownSymbol c) => Predicate (SymEqualTo c) Text where
  validate proxy txt
    | txt == sym = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      sym = T.pack $ TL.symbolVal @c Proxy
      err = txt <> " does not equal the predicate"

-- | @since 0.1.0.0
instance (KnownSymbol c) => Predicate (SymEqualTo c) LT.Text where
  validate proxy txt
    | txt' == sym = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      txt' = LT.toStrict txt
      sym = T.pack $ TL.symbolVal @c Proxy
      err = txt' <> " does not equal the predicate"

-- $char
-- This section models the boolean functions defined in "Data.Char". See
-- that module for more information regarding these definitions.
--
-- These instances are defined for 'Char' and 'Word8', though they can be
-- extended to 'String', 'Text', and 'Data.ByteString.ByteString' via
-- 'Refined.Extra.Predicates.Foldable.All'.
--
-- Note: Although 'Char' has instances for all of these predicates, some are
-- missing for 'Word8' (and by extension 'Data.ByteString.ByteString').
-- This is due to 'Word8'\'s size restriction, i.e., a single byte
-- @0 <= b <= 255@. Thus, predicates that extend over the entirety of the
-- unicode range (e.g. 'Alpha') do not have 'Word8' instances, as this could be
-- misleading. For instance, consider the unicode character "ɦ" (U+0266):
--
-- >>> :{
--   let txt = T.singleton ch -- ch == 0x0266 i.e. ɦ
--    in isRight $ refine @(All Alpha) txt
-- :}
-- True
--
-- This 'Char' is part of the alpha unicode category, so the refinement
-- succeeds. On the other hand, suppose we have a 'Word8' instance that
-- performs the obvious @'C.isAlpha' . 'BS.w2c'@:
--
-- >>> :{
--   let bs = encodeUtf8 $ T.singleton ch
--    in isRight $ refine @(All Alpha) bs
-- :}
-- False
--
-- The problem is that we are checking the underlying bytes if they satisfy
-- 'C.isAlpha', but this is only true for Ascii alpha characters. Morally, our
-- bytestring is this structure:
--
-- >>> BS.foldr (:) [] (encodeUtf8 $ T.singleton ch)
-- [201,166]
--
-- Our 'Alpha' refinement fails because the individual byte components of
-- "ɦ" are not themselves considered "alpha" characters (indeed this will only
-- occur due to chance). These are the options:
--
-- 1. Outlaw 'Word8'/'Data.ByteString.ByteString' instances completely.
-- 2. Provide the naive @'C.isAlpha' . 'BS.w2c'@ implementation for 'Word8'.
-- 3. Implement 'Data.ByteString.ByteString' instances by first converting to
--    'Text', i.e., do not use its underlying fold.
-- 4. Provide 'Word8' instances only when they coincide with 'Char' (i.e.
--    ascii/latin1 predicates). In this case, 'Data.ByteString.ByteString'
--    works as expected; that is, we can make assertions based on the
--    underlying bytes, but nothing that requires a specific encoding, and we
--    do not get surprised by 'Text'/'Data.ByteString.ByteString' mismatches.
--
-- Of these, only one and four are reasonable. Two is out because it can have
-- confusing semantics (illustrated above).
--
-- Three is rejected because the API is no longer consistent, and we
-- have to arbitrarily assume the 'Data.ByteString.ByteString' shares its
-- 'Text' encoding (i.e. UTF-8).
--
-- One is defensible, but we choose option four, reasoning that it could be
-- useful to assert that a given bytestring contains only ascii numbers or
-- alpha characters while avoiding the pitfalls of reusing predicates intended
-- for arbitrary unicode.

-- $unicode
-- These predicates are for unicode code points, hence they are available for
-- 'Char' (thus 'String', 'Text').

-- | Predicate for a 'C.isSpace'.
--
-- ==== __Examples__
-- >>> validate @Space Proxy ' '
-- Nothing
-- >>> validate @Space Proxy '\r'
-- Nothing
--
-- >>> showRefineException <$> validate @Space Proxy 'a'
-- Just "RefineOtherException (Space) \"a is not a space character\""
--
-- @since 0.1.0.0
type Space :: Type
data Space
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Space Char where
  validate proxy c
    | C.isSpace c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not a space character"

-- | Predicate for 'C.isLower'.
--
-- ==== __Examples__
-- >>> validate @Lower Proxy 'c'
-- Nothing
--
-- >>> showRefineException <$> validate @Lower Proxy 'C'
-- Just "RefineOtherException (Lower) \"C is not lowercase\""
--
-- @since 0.1.0.0
type Lower :: Type
data Lower
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Lower Char where
  validate proxy c
    | C.isLower c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not lowercase"

-- | Predicate for 'C.isUpper'.
--
-- ==== __Examples__
-- >>> validate @Upper Proxy 'C'
-- Nothing
--
-- >>> showRefineException <$> validate @Upper Proxy 'c'
-- Just "RefineOtherException (Upper) \"c is not uppercase\""
--
-- @since 0.1.0.0
type Upper :: Type
data Upper
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Upper Char where
  validate proxy c
    | C.isUpper c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not uppercase"

-- | Predicate for 'C.isAlpha'.
--
-- ==== __Examples__
-- >>> validate @Alpha Proxy 'c'
-- Nothing
--
-- >>> validate @Alpha Proxy 'ɦ'
-- Nothing
--
-- >>> showRefineException <$> validate @Alpha Proxy '7'
-- Just "RefineOtherException (Alpha) \"7 is not an alphabetic character\""
--
-- @since 0.1.0.0
type Alpha :: Type
data Alpha
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Alpha Char where
  validate proxy c
    | C.isAlpha c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not an alphabetic character"

-- | Predicate for 'C.isAlphaNum'.
--
-- ==== __Examples__
-- >>> validate @AlphaNum Proxy 'a'
-- Nothing
--
-- >>> validate @AlphaNum Proxy '1'
-- Nothing
--
-- >>> showRefineException <$> validate @AlphaNum Proxy '!'
-- Just "RefineOtherException (AlphaNum) \"! is not an alpha-numeric character\""
--
-- @since 0.1.0.0
type AlphaNum :: Type
data AlphaNum
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate AlphaNum Char where
  validate proxy c
    | C.isAlphaNum c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not an alpha-numeric character"

-- | Predicate for 'C.isPrint'.
--
-- ==== __Examples__
-- >>> validate @Print Proxy 'a'
-- Nothing
--
-- >>> showRefineException <$> validate @Print Proxy '\v'
-- Just "RefineOtherException (Print) \"\\v is not a printable character\""
--
-- @since 0.1.0.0
type Print :: Type
data Print
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Print Char where
  validate proxy c
    | C.isPrint c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not a printable character"

-- | Predicate for 'C.isLetter'.
--
-- ==== __Examples__
-- >>> validate @Letter Proxy 'f'
-- Nothing
--
-- >>> showRefineException <$> validate @Letter Proxy '\r'
-- Just "RefineOtherException (Letter) \"\\r is not a letter\""
--
-- @since 0.1.0.0
type Letter :: Type
data Letter
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Letter Char where
  validate proxy c
    | C.isLetter c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not a letter"

-- | Predicate for 'C.isMark'.
--
-- ==== __Examples__
-- >>> validate @Mark Proxy '\x20DD'
-- Nothing
--
-- >>> showRefineException <$> validate @Mark Proxy 'a'
-- Just "RefineOtherException (Mark) \"a is not a mark\""
--
-- @since 0.1.0.0
type Mark :: Type
data Mark
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Mark Char where
  validate proxy c
    | C.isMark c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not a mark"

-- | Predicate for 'C.isNumber'.
--
-- ==== __Examples__
-- >>> validate @Number Proxy '2'
-- Nothing
--
-- >>> showRefineException <$> validate @Number Proxy 'a'
-- Just "RefineOtherException (Number) \"a is not a number\""
--
-- @since 0.1.0.0
type Number :: Type
data Number
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Number Char where
  validate proxy c
    | C.isNumber c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not a number"

-- | Predicate for 'C.isPunctuation'.
--
-- ==== __Examples__
-- >>> validate @Punctuation Proxy '!'
-- Nothing
--
-- >>> showRefineException <$> validate @Punctuation Proxy 'a'
-- Just "RefineOtherException (Punctuation) \"a is not punctuation\""
--
-- @since 0.1.0.0
type Punctuation :: Type
data Punctuation
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Punctuation Char where
  validate proxy c
    | C.isPunctuation c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not punctuation"

-- | Predicate for 'C.isSymbol'.
--
-- ==== __Examples__
-- >>> validate @Symbol Proxy '$'
-- Nothing
--
-- >>> showRefineException <$> validate @Symbol Proxy 'a'
-- Just "RefineOtherException (Symbol) \"a is not a symbol\""
--
-- @since 0.1.0.0
type Symbol :: Type
data Symbol
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Symbol Char where
  validate proxy c
    | C.isSymbol c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not a symbol"

-- | Predicate for 'C.isSeparator'.
--
-- ==== __Examples__
-- >>> validate @Separator Proxy ' '
-- Nothing
--
-- >>> showRefineException <$> validate @Separator Proxy 'a'
-- Just "RefineOtherException (Separator) \"a is not a separator\""
--
-- @since 0.1.0.0
type Separator :: Type
data Separator
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Separator Char where
  validate proxy c
    | C.isSeparator c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not a separator"

-- $ascii
-- These predicates are for ascii/latin1. Thus they will work for 'Char'
-- (and 'String', 'Text') /and/ 'Word8' (hence 'Data.ByteString.ByteString').

-- | Predicate for 'C.isControl'.
--
-- ==== __Examples__
-- >>> validate @Control Proxy '\r'
-- Nothing
--
-- >>> showRefineException <$> validate @Control Proxy 'a'
-- Just "RefineOtherException (Control) \"a is not a control character\""
--
-- @since 0.1.0.0
type Control :: Type
data Control
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Control Char where
  validate proxy c
    | C.isControl c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not a control character"

-- | @since 0.1.0.0
instance Predicate Control Word8 where
  validate proxy w
    | C.isControl c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      c = BS.w2c w
      err = T.singleton c <> " is not a control character"

-- | Predicate for 'C.isDigit'.
--
-- ==== __Examples__
-- >>> validate @Digit Proxy '1'
-- Nothing
--
-- >>> showRefineException <$> validate @Digit Proxy 'a'
-- Just "RefineOtherException (Digit) \"a is not a digit\""
--
-- @since 0.1.0.0
type Digit :: Type
data Digit
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Digit Char where
  validate proxy c
    | C.isDigit c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not a digit"

-- | @since 0.1.0.0
instance Predicate Digit Word8 where
  validate proxy w
    | C.isDigit c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      c = BS.w2c w
      err = T.singleton c <> " is not a digit"

-- | Predicate for 'C.isOctDigit'.
--
-- ==== __Examples__
-- >>> validate @OctDigit Proxy '4'
-- Nothing
--
-- >>> showRefineException <$> validate @OctDigit Proxy '9'
-- Just "RefineOtherException (OctDigit) \"9 is not an octal digit\""
--
-- @since 0.1.0.0
type OctDigit :: Type
data OctDigit
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate OctDigit Char where
  validate proxy c
    | C.isOctDigit c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not an octal digit"

-- | @since 0.1.0.0
instance Predicate OctDigit Word8 where
  validate proxy w
    | C.isOctDigit c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      c = BS.w2c w
      err = T.singleton c <> " is not an octal digit"

-- | Predicate for 'C.isHexDigit'.
--
-- ==== __Examples__
-- >>> validate @HexDigit Proxy '1'
-- Nothing
--
-- >>> validate @HexDigit Proxy 'f'
-- Nothing
--
-- >>> showRefineException <$> validate @HexDigit Proxy 'g'
-- Just "RefineOtherException (HexDigit) \"g is not a hexadecimal digit\""
--
-- @since 0.1.0.0
type HexDigit :: Type
data HexDigit
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate HexDigit Char where
  validate proxy c
    | C.isHexDigit c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not a hexadecimal digit"

-- | @since 0.1.0.0
instance Predicate HexDigit Word8 where
  validate proxy w
    | C.isHexDigit c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      c = BS.w2c w
      err = T.singleton c <> " is not a hexadecimal digit"

-- | Predicate for 'C.isAscii'.
--
-- ==== __Examples__
-- >>> validate @Ascii Proxy 'a'
-- Nothing
--
-- >>> showRefineException <$> validate @Ascii Proxy '\x20DD'
-- Just "RefineOtherException (Ascii) \"\\8413 is not ascii\""
--
-- @since 0.1.0.0
type Ascii :: Type
data Ascii
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Ascii Char where
  validate proxy c
    | C.isAscii c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not ascii"

-- | @since 0.1.0.0
instance Predicate Ascii Word8 where
  validate proxy w
    | C.isAscii c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      c = BS.w2c w
      err = T.singleton c <> " is not ascii"

-- | Predicate for 'C.Latin1'.
--
-- ==== __Examples__
-- >>> validate @Latin1 Proxy 'a'
-- Nothing
--
-- >>> showRefineException <$> validate @Latin1 Proxy '\x20DD'
-- Just "RefineOtherException (Latin1) \"\\8413 is not latin1\""
--
-- @since 0.1.0.0
type Latin1 :: Type
data Latin1
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate Latin1 Char where
  validate proxy c
    | C.isLatin1 c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not latin1"

-- | @since 0.1.0.0
instance Predicate Latin1 Word8 where
  validate proxy w
    | C.isLatin1 c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      c = BS.w2c w
      err = T.singleton c <> " is not latin1"

-- | Predicate for 'C.isAsciiUpper'.
--
-- ==== __Examples__
-- >>> validate @AsciiUpper Proxy 'A'
-- Nothing
--
-- >>> showRefineException <$> validate @AsciiUpper Proxy 'a'
-- Just "RefineOtherException (AsciiUpper) \"a is not uppercase ascii\""
--
-- @since 0.1.0.0
type AsciiUpper :: Type
data AsciiUpper
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate AsciiUpper Char where
  validate proxy c
    | C.isAsciiUpper c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not uppercase ascii"

-- | @since 0.1.0.0
instance Predicate AsciiUpper Word8 where
  validate proxy w
    | C.isAsciiUpper c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      c = BS.w2c w
      err = T.singleton c <> " is not uppercase ascii"

-- | Predicate for 'C.isAsciiLower'.
--
-- ==== __Examples__
-- >>> validate @AsciiLower Proxy 'a'
-- Nothing
--
-- >>> showRefineException <$> validate @AsciiLower Proxy 'A'
-- Just "RefineOtherException (AsciiLower) \"A is not lowercase ascii\""
--
-- @since 0.1.0.0
type AsciiLower :: Type
data AsciiLower
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate AsciiLower Char where
  validate proxy c
    | C.isAsciiLower c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not lowercase ascii"

-- | @since 0.1.0.0
instance Predicate AsciiLower Word8 where
  validate proxy w
    | C.isAsciiLower c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      c = BS.w2c w
      err = T.singleton c <> " is not lowercase ascii"

-- | Predicate for 'C.isAscii' and 'C.isAlpha', primarily intended for 'Word8'.
-- Redundant for 'Char', as this is equivalent to @(Ascii && Alpha)@, but we
-- include 'Char' for completeness.
--
-- ==== __Examples__
-- >>> validate @AsciiAlpha Proxy 'a'
-- Nothing
--
-- >>> showRefineException <$> validate @AsciiAlpha Proxy '1'
-- Just "RefineOtherException (AsciiAlpha) \"1 is not alpha ascii\""
--
-- >>> showRefineException <$> validate @AsciiAlpha Proxy 'ɦ'
-- Just "RefineOtherException (AsciiAlpha) \"\\614 is not alpha ascii\""
--
-- @since 0.1.0.0
type AsciiAlpha :: Type
data AsciiAlpha
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate AsciiAlpha Char where
  validate proxy c
    | C.isAscii c && C.isAlpha c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not alpha ascii"

-- | @since 0.1.0.0
instance Predicate AsciiAlpha Word8 where
  validate proxy w
    | C.isAscii c && C.isAlpha c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      c = BS.w2c w
      err = T.singleton c <> " is not alpha ascii"

-- | Predicate for 'C.isAscii' and 'C.isAlphaNum', primarily intended for
-- 'Word8'. Redundant for 'Char', as this is equivalent to
-- @(Ascii && AlphaNum)@, but we include 'Char' for completeness.
--
-- ==== __Examples__
-- >>> validate @AsciiAlphaNum Proxy 'a'
-- Nothing
--
-- >>> validate @AsciiAlphaNum Proxy '1'
-- Nothing
--
-- >>> showRefineException <$> validate @AsciiAlphaNum Proxy '1'
-- Nothing
--
-- >>> showRefineException <$> validate @AsciiAlphaNum Proxy 'ɦ'
-- Just "RefineOtherException (AsciiAlphaNum) \"\\614 is not alpha-numeric ascii\""
--
-- @since 0.1.0.0
type AsciiAlphaNum :: Type
data AsciiAlphaNum
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance Predicate AsciiAlphaNum Char where
  validate proxy c
    | C.isAscii c && C.isAlphaNum c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      err = T.singleton c <> " is not alpha-numeric ascii"

-- | @since 0.1.0.0
instance Predicate AsciiAlphaNum Word8 where
  validate proxy w
    | C.isAscii c && C.isAlphaNum c = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      c = BS.w2c w
      err = T.singleton c <> " is not alpha-numeric ascii"
