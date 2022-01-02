-- | Predicates for 'Text' and 'String'.
--
-- @since 0.1.0.0
module Refined.Extras.Predicates.Text
  ( -- * Symbol Equality
    SymEqualTo,

    -- * Char Predicates
    -- $char

    -- ** Character Classification
    Control,
    Space,
    Lower,
    Upper,
    Alpha,
    AlphaNum,
    Digit,
    OctDigit,
    HexDigit,
    Letter,
    Mark,
    Number,
    Punctuation,
    Symbol,
    Separator,

    -- *** Subranges
    Ascii,
    Latin1,
    AsciiUpper,
    AsciiLower,
  )
where

import Data.Char qualified as C
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable qualified as Ty
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol)
import GHC.TypeLits qualified as TL
import Refined (Predicate (..), RefineException (..))

-- $setup
-- >>> import Refined.Extras.Predicates.Foldable (All)
-- >>> import Refined.Extras.Utils (showRefineException)

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
  deriving
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance KnownSymbol c => Predicate (SymEqualTo c) Char where
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
instance KnownSymbol c => Predicate (SymEqualTo c) String where
  validate proxy txt
    | txt == sym = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      sym = TL.symbolVal @c Proxy
      err = T.pack $ txt <> " does not equal the predicate"

-- | @since 0.1.0.0
instance KnownSymbol c => Predicate (SymEqualTo c) Text where
  validate proxy txt
    | txt == sym = Nothing
    | otherwise = Just $ RefineOtherException (Ty.typeRep proxy) err
    where
      sym = T.pack $ TL.symbolVal @c Proxy
      err = txt <> " does not equal the predicate"

-- $char
-- This section models the boolean functions defined in "Data.Char". See
-- that module for more information regarding these definitions.
--
-- Although the instances are defined for 'Char', they can be
-- extended to 'String' and 'Text' via "Refined.Predicates.Foldable"'s
-- 'Refined.Predicates.Foldable.All'.
--
-- ==== __Examples__
-- >>> validate @(All Alpha) Proxy "abc"
-- Nothing
--
-- >>> showRefineException <$> validate @(All Alpha) Proxy "abc1"
-- Just "RefineOtherException (Alpha) \"1 is not an alphabetic character\""

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
  deriving
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
  deriving
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
  deriving
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
  deriving
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
-- >>> showRefineException <$> validate @Alpha Proxy '7'
-- Just "RefineOtherException (Alpha) \"7 is not an alphabetic character\""
--
-- @since 0.1.0.0
type Alpha :: Type
data Alpha
  deriving
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
  deriving
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
  deriving
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
  deriving
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
  deriving
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

-- | Predicate for 'C.isHexDigit'.
--
-- >>> validate @HexDigit Proxy '1'
-- Nothing
--
-- ==== __Examples__
-- >>> validate @HexDigit Proxy 'f'
-- Nothing
--
-- >>> showRefineException <$> validate @HexDigit Proxy 'g'
-- Just "RefineOtherException (HexDigit) \"g is not a hexadecimal digit\""
--
-- @since 0.1.0.0
type HexDigit :: Type
data HexDigit
  deriving
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
  deriving
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
  deriving
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
  deriving
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
  deriving
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
  deriving
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
  deriving
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
  deriving
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
  deriving
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
  deriving
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
  deriving
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