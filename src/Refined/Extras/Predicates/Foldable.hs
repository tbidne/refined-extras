-- | Predicates for 'Foldable'.
--
-- @since 0.1.0.0
module Refined.Extras.Predicates.Foldable
  ( -- * Elements
    All,
    Any,
    None,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Data.Text.Lazy qualified as LT
import Data.Typeable (Proxy (Proxy))
import Data.Typeable qualified as Ty
import Data.Word (Word8)
import GHC.Generics (Generic)
import Refined
  ( Not,
    Predicate (validate),
    RefineException (RefineOtherException),
  )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Refined (NonZero, Negative)
-- >>> import Refined.Extras.Utils (showRefineException)

-- | Predicate for all elements satisfying some predicate.
--
-- ==== __Examples__
-- >>> validate @(All NonZero) Proxy [1..5]
-- Nothing
--
-- >>> showRefineException <$> validate @(All NonZero) Proxy [0..5]
-- Just "RefineOtherException (NotEqualTo 0) \"Value does equal 0\""
--
-- @since 0.1.0.0
type All :: Type -> Type
data All p
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance (Foldable f, Predicate p a) => Predicate (All p) (f a) where
  validate _ = allFoldableSatisfies (validate @p Proxy)

-- | @since 0.1.0.0
instance (Predicate p Char) => Predicate (All p) Text where
  validate _ = allTextSatisfies (validate @p Proxy)

-- | @since 0.1.0.0
instance (Predicate p Char) => Predicate (All p) LT.Text where
  validate _ = allLazyTextSatisfies (validate @p Proxy)

-- | @since 0.1.0.0
instance (Predicate p Word8) => Predicate (All p) ByteString where
  validate _ = allByteStringSatisfies (validate @p Proxy)

-- | @since 0.1.0.0
instance (Predicate p Word8) => Predicate (All p) BSL.ByteString where
  validate _ = allLazyByteStringSatisfies (validate @p Proxy)

-- | Predicate for any element satisfying some predicate.
--
-- ==== __Examples__
-- >>> validate @(Any NonZero) Proxy [0,0,0,4]
-- Nothing
--
-- >>> showRefineException <$> validate @(Any NonZero) Proxy [0,0,0]
-- Just "RefineOtherException (NotEqualTo 0) \"No element satisfied the predicate\""
--
-- @since 0.1.0.0
type Any :: Type -> Type
data Any p
  deriving stock
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance (Foldable f, Predicate p a) => Predicate (Any p) (f a) where
  validate _ = anyFoldableSatisfies err (validate proxy)
    where
      proxy = Proxy @p
      err = RefineOtherException (Ty.typeRep proxy) "No element satisfied the predicate"

-- | @since 0.1.0.0
instance (Predicate p Char) => Predicate (Any p) Text where
  validate _ txt = anyTextSatisfies err (validate proxy) txt
    where
      proxy = Proxy @p
      msg = "No element satisfied the predicate: " <> txt
      err = RefineOtherException (Ty.typeRep proxy) msg

-- | @since 0.1.0.0
instance (Predicate p Char) => Predicate (Any p) LT.Text where
  validate _ txt = anyLazyTextSatisfies err (validate proxy) txt
    where
      proxy = Proxy @p
      msg = "No element satisfied the predicate: " <> txt
      err = RefineOtherException (Ty.typeRep proxy) (LT.toStrict msg)

-- | @since 0.1.0.0
instance (Predicate p Word8) => Predicate (Any p) ByteString where
  validate _ bs = anyByteStringSatisfies err (validate proxy) bs
    where
      proxy = Proxy @p
      prefix = "No element satisfied the predicate: "
      err = RefineOtherException (Ty.typeRep proxy) (prefix <> msg)
      msg = decodeUtf8Lenient bs

-- | @since 0.1.0.0
instance (Predicate p Word8) => Predicate (Any p) BSL.ByteString where
  validate _ bs = anyLazyByteStringSatisfies err (validate proxy) bs
    where
      proxy = Proxy @p
      prefix = "No element satisfied the predicate: "
      err = RefineOtherException (Ty.typeRep proxy) (prefix <> msg)
      msg = decodeUtf8Lenient (BSL.toStrict bs)

-- | Predicate for no elements satisfying a predicate.
--
-- ==== __Examples__
-- >>> validate @(None Negative) Proxy [3,4,5]
-- Nothing
--
-- >>> showRefineException <$> validate @(None Negative) Proxy [3,-1,2,5]
-- Just "RefineNotException (Not * (Any (LessThan 0)))"
--
-- @since 0.1.0.0
type None :: Type -> Type
type None p = Not (Any p)

allFoldableSatisfies :: (Foldable f) => (a -> Maybe b) -> f a -> Maybe b
allFoldableSatisfies = allSatisfies foldr

allTextSatisfies :: (Char -> Maybe b) -> Text -> Maybe b
allTextSatisfies = allSatisfies T.foldr

allLazyTextSatisfies :: (Char -> Maybe b) -> LT.Text -> Maybe b
allLazyTextSatisfies = allSatisfies LT.foldr

allByteStringSatisfies :: (Word8 -> Maybe a) -> ByteString -> Maybe a
allByteStringSatisfies = allSatisfies BS.foldr

allLazyByteStringSatisfies :: (Word8 -> Maybe a) -> BSL.ByteString -> Maybe a
allLazyByteStringSatisfies = allSatisfies BSL.foldr

allSatisfies :: ((a -> Maybe b -> Maybe b) -> Maybe c -> d) -> (a -> Maybe b) -> d
allSatisfies foldFn testPred = foldFn (\x acc -> testPred x <|> acc) Nothing

anyFoldableSatisfies :: (Foldable f) => b -> (a -> Maybe b) -> f a -> Maybe b
anyFoldableSatisfies = anySatisfies foldr

anyTextSatisfies :: b -> (Char -> Maybe b) -> Text -> Maybe b
anyTextSatisfies = anySatisfies T.foldr

anyLazyTextSatisfies :: b -> (Char -> Maybe b) -> LT.Text -> Maybe b
anyLazyTextSatisfies = anySatisfies LT.foldr

anyByteStringSatisfies :: b -> (Word8 -> Maybe b) -> ByteString -> Maybe b
anyByteStringSatisfies = anySatisfies BS.foldr

anyLazyByteStringSatisfies :: b -> (Word8 -> Maybe b) -> BSL.ByteString -> Maybe b
anyLazyByteStringSatisfies = anySatisfies BSL.foldr

anySatisfies :: ((a -> Maybe b -> Maybe b) -> Maybe c -> d) -> c -> (a -> Maybe e) -> d
anySatisfies foldFn defErr testPred = foldFn f (Just defErr)
  where
    f x acc = case testPred x of
      Just _ -> acc
      Nothing -> Nothing

-- | Because text used by GHC < 9.4 does not have decodeUtf8Lenient,
-- apparently.
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TEnc.decodeUtf8With TEncError.lenientDecode
