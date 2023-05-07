-- | Exports generators.
--
-- @since 0.1.0.0
module Gens.Text
  ( genChar,
    genWord8X,
    genStringX,
    genTextX,
    genLazyTextX,

    -- * Text
    genTextAlpha,
    genTextAlphaWithDigit,
    genLazyTextAlpha,
    genLazyTextAlphaWithDigit,

    -- * ByteString
    genByteStringDigit,
    genByteStringDigitWithAlpha,
    genLazyByteStringDigit,
    genLazyByteStringDigitWithAlpha,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as C
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Lazy qualified as LT
import Data.Word (Word8)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR

genChar :: (MonadGen m) => m Char
genChar = HG.unicodeAll

genWord8X :: (MonadGen m) => m Char -> m Word8
genWord8X genFn = fromIntegral . C.ord <$> genFn

genStringX :: (MonadGen m) => m Char -> m String
genStringX = HG.string maxStrSz

genTextX :: (MonadGen m) => m Char -> m Text
genTextX = HG.text maxStrSz

genLazyTextX :: (MonadGen m) => m Char -> m LT.Text
genLazyTextX = fmap LT.fromStrict . HG.text maxStrSz

-- Note: Hedgehog's notion of 'alpha' is specifically ASCII alpha. This is
-- much more limited than unicode's notion of alpha. Thus we can use this
-- whenever we want to satisfy our Alpha predicate, keeping in mind that
-- are only covering a fraction of the domain.
genTextAlpha :: (MonadGen m) => m Text
genTextAlpha = genTextX HG.alpha

genLazyTextAlpha :: (MonadGen m) => m LT.Text
genLazyTextAlpha = LT.fromStrict <$> genTextAlpha

genTextAlphaWithDigit :: (MonadGen m) => m Text
genTextAlphaWithDigit = do
  txt <- genTextX HG.alpha
  d <- HG.digit
  let str = d : T.unpack txt
  T.pack <$> HG.shuffle str

genLazyTextAlphaWithDigit :: (MonadGen m) => m LT.Text
genLazyTextAlphaWithDigit = LT.fromStrict <$> genTextAlphaWithDigit

genByteStringDigit :: (MonadGen m) => m ByteString
genByteStringDigit = TEnc.encodeUtf8 <$> genTextX HG.digit

genLazyByteStringDigit :: (MonadGen m) => m LBS.ByteString
genLazyByteStringDigit = LBS.fromStrict <$> genByteStringDigit

genByteStringDigitWithAlpha :: (MonadGen m) => m ByteString
genByteStringDigitWithAlpha = do
  digits <- genTextX HG.digit
  c <- HG.alpha
  let s = c : T.unpack digits
  TEnc.encodeUtf8 . T.pack <$> HG.shuffle s

genLazyByteStringDigitWithAlpha :: (MonadGen m) => m LBS.ByteString
genLazyByteStringDigitWithAlpha = LBS.fromStrict <$> genByteStringDigitWithAlpha

maxStrSz :: (Integral a) => HR.Range a
maxStrSz = HR.exponential 0 100
