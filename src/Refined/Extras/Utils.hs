-- | Utilities for "Refined".
--
-- @since 0.1.0.0
module Refined.Extras.Utils
  ( -- * Pattern Synonym
    pattern MkRefined,

    -- * Exception Functions
    showRefineException,
    showtRefineException,
    refineExceptionToType,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.These (These (..))
import Data.Typeable (TypeRep)
import Refined (RefineException (..))
import Refined.Unsafe.Type (Refined (..))

-- $setup
-- >>> :set -XAllowAmbiguousTypes
-- >>> :set -XTemplateHaskell
-- >>> import Data.Bifunctor (Bifunctor (..))
-- >>> import Refined (And, NonNegative, NonZero, refine, Xor, refineTH)
-- >>> import Refined.Extras.Polymorphism (Implies)

-- | Unidirectional pattern synonym for 'Refined'. This allows us to pattern
-- match on a refined term without exposing the unsafe internal details.
--
-- ==== __Examples__
-- >>> :{
-- let safeDiv :: Implies p NonZero => Int -> Refined p Int -> Int
--     safeDiv n (MkRefined d) = n `div` d
--     two = $$(refineTH @NonZero @Int 2)
--  in safeDiv 10 two
-- :}
-- 5
--
-- @since 0.1.0.0
pattern MkRefined :: a -> Refined p a
pattern MkRefined a <- Refined a

{-# COMPLETE MkRefined #-}

-- | Displays a 'RefineException' without formatting. Intended for situations
-- where 'RefineException'\'s default formatting is undesirable
-- (e.g. doctests, logging).
--
-- ==== __Examples__
-- >>> first showRefineException $ refine @(And NonZero NonNegative) 0
-- Left "RefineAndException (And * * (NotEqualTo 0) (From 0)) (This (RefineOtherException (NotEqualTo 0) \"Value does equal 0\"))"
--
-- >>> let ex = refine @(Xor (And NonZero NonNegative) NonZero) 0
-- >>> first showRefineException ex
-- Left "RefineXorException (Xor * * (And * * (NotEqualTo 0) (From 0)) (NotEqualTo 0)) (RefineAndException (And * * (NotEqualTo 0) (From 0)) (This (RefineOtherException (NotEqualTo 0) \"Value does equal 0\"))) (RefineOtherException (NotEqualTo 0) \"Value does equal 0\")"
--
-- @since 0.1.0.0
showRefineException :: RefineException -> String
showRefineException = T.unpack . showtRefineException

-- | Variant of showRefineException for 'Text'.
--
-- @since 0.1.0.0
showtRefineException :: RefineException -> Text
showtRefineException (RefineNotException ty) = "RefineNotException " <> showParens ty
showtRefineException (RefineAndException ty th) =
  "RefineAndException " <> showParens ty <> " (" <> showThese th <> ")"
showtRefineException (RefineOrException ty e1 e2) =
  "RefineOrException "
    <> showParens ty
    <> " ("
    <> showtRefineException e1
    <> ") ("
    <> showtRefineException e2
    <> ")"
showtRefineException (RefineXorException ty mEx) =
  "RefineXorException " <> showParens ty <> case mEx of
    Nothing -> " Nothing"
    Just (e1, e2) ->
      " ("
        <> showtRefineException e1
        <> ") ("
        <> showtRefineException e2
        <> ")"
showtRefineException (RefineSomeException ty someEx) =
  "RefineSomeException " <> showParens ty <> " (" <> T.pack (show someEx) <> ")"
showtRefineException (RefineOtherException ty txt) =
  "RefineOtherException " <> showParens ty <> " " <> T.pack (show txt)

-- | Retrieves the 'TypeRep' corresponding to a 'RefineException'.
--
-- ==== __Examples__
-- >>> first refineExceptionToType $ refine @(And NonZero NonNegative) 0
-- Left (And * * (NotEqualTo 0) (From 0))
--
-- @since 0.1.0.0
refineExceptionToType :: RefineException -> TypeRep
refineExceptionToType (RefineNotException ty) = ty
refineExceptionToType (RefineAndException ty _) = ty
refineExceptionToType (RefineOrException ty _ _) = ty
refineExceptionToType (RefineXorException ty _) = ty
refineExceptionToType (RefineSomeException ty _) = ty
refineExceptionToType (RefineOtherException ty _) = ty

showParens :: (Show a) => a -> Text
showParens ty = "(" <> T.pack (show ty) <> ")"

showThese :: These RefineException RefineException -> Text
showThese (These e1 e2) =
  "These ("
    <> showtRefineException e1
    <> ") ("
    <> showtRefineException e2
    <> ")"
showThese (This e1) =
  "This ("
    <> showtRefineException e1
    <> ")"
showThese (That e2) =
  "That ("
    <> showtRefineException e2
    <> ")"
