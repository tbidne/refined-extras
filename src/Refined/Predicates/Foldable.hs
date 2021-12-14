-- | Provides predicates for 'Foldable'.
--
-- @since 0.1.0.0
module Refined.Predicates.Foldable
  ( -- * Elements
    All,
    Any,
    None,
  )
where

import Control.Applicative (Alternative (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Proxy (..), Typeable)
import Data.Typeable qualified as Ty
import GHC.Generics (Generic)
import Refined (Not, Predicate (..), RefineException (..))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Refined (NonZero, Negative)
-- >>> :{
--   trimShow :: Show a => a -> String
--   trimShow = T.unpack . T.strip . T.pack . show
-- :}

-- | Predicate for all elements satisfying some predicate.
--
-- ==== __Examples__
-- >>> validate @(All NonZero) Proxy [1..5]
-- Nothing
--
-- >>> trimShow <$> validate @(All NonZero) Proxy [0..5]
-- Just "The predicate (NotEqualTo 0) failed with the message: Value does equal 0"
--
-- @since 0.1.0.0
type All :: Type -> Type
data All p
  deriving
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance (Foldable f, Predicate p a) => Predicate (All p) (f a) where
  validate _ xs = allSatisfies (validate @p Proxy) xs

-- | @since 0.1.0.0
instance Predicate p Char => Predicate (All p) Text where
  validate _ = allTextSatisfies (validate @p Proxy)

-- | Predicate for any element satisfying some predicate.
--
-- ==== __Examples__
-- >>> validate @(Any NonZero) Proxy [0,0,0,4]
-- Nothing
--
-- >>> trimShow <$> validate @(Any NonZero) Proxy [0,0,0]
-- Just "The predicate (NotEqualTo 0) failed with the message: No element satisfied the predicate"
--
-- @since 0.1.0.0
type Any :: Type -> Type
data Any p
  deriving
    ( -- | @since 0.1.0.0
      Generic
    )

-- | @since 0.1.0.0
instance (Foldable f, Predicate p a, Typeable p) => Predicate (Any p) (f a) where
  validate _ xs = anySatisfies err (validate proxy) xs
    where
      proxy = Proxy @p
      err = RefineOtherException (Ty.typeRep proxy) "No element satisfied the predicate"

-- | @since 0.1.0.0
instance (Predicate p Char, Typeable p) => Predicate (Any p) Text where
  validate _ txt = anyTextSatisfies err (validate proxy) txt
    where
      proxy = Proxy @p
      msg = "No element satisfied the predicate: " <> txt
      err = RefineOtherException (Ty.typeRep proxy) msg

-- | Predicate for no elements satisfying a predicate.
--
-- ==== __Examples__
-- >>> validate @(None Negative) Proxy [3,4,5]
-- Nothing
--
-- >>> trimShow <$> validate @(None Negative) Proxy [3,-1,2,5]
-- Just "The predicate (Not (Any (LessThan 0))) does not hold"
--
-- @since 0.1.0.0
type None :: Type -> Type

type None p = Not (Any p)

allSatisfies :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
allSatisfies p = foldr (\x acc -> p x <|> acc) Nothing

allTextSatisfies :: (Char -> Maybe b) -> Text -> Maybe b
allTextSatisfies p = T.foldr (\x acc -> p x <|> acc) Nothing

anySatisfies :: Foldable f => b -> (a -> Maybe b) -> f a -> Maybe b
anySatisfies defErr p = foldr f (Just defErr)
  where
    f x acc = case p x of
      Just _ -> acc
      Nothing -> Nothing

anyTextSatisfies :: b -> (Char -> Maybe b) -> Text -> Maybe b
anyTextSatisfies defErr p = T.foldr f (Just defErr)
  where
    f x acc = case p x of
      Just _ -> acc
      Nothing -> Nothing
