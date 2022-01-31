{-# LANGUAGE UndecidableInstances #-}

-- | Type families for defining functions that are polymorphic over
-- predicates.
--
-- @since 0.1.0.0
module Refined.Extras.Polymorphism
  ( -- * Type Families
    -- $polymorphism
    Implies,
    (:=>),
    ImpliesBool,

    -- * Errors
    ErrIfFalse,
    PredNotFound,
  )
where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Refined.Extras.Polymorphism.Internal (ImpliesCNF, ToCNF)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Data.Text (Text, splitOn)
-- >>> import Refined (NonEmpty, Not, Refined, refineTH, unrefine, type (&&), type (||))
-- >>> import Refined (NonZero, NonNegative, Positive)
-- >>> data A
-- >>> data B
-- >>> data C

-- $polymorphism
-- The 'Implies' type family lets us write functions that are polymorphic
-- over predicates, i.e., we can define functions that require some predicate
-- @p@, which can then be used by /any/ refined term, as long as the term
-- satisfies @p@ . This means we can write APIs that are both safe /and/
-- flexible.

-- | @'Implies' q p@ raises a type error if @q@ does not logically imply
-- @p@. See 'ImpliesBool' for when @q@ implies @p@.
--
-- ==== __Examples__
-- >>> :{
--   let safeDiv :: (Implies p NonZero, Integral n) => n -> Refined p n -> n
--       safeDiv x d = x `div` unrefine d
--       --
--       -- term does not have to exactly match Refined '[NonZero] n
--       d :: Refined (NonZero && Positive) Int
--       d = $$(refineTH 7)
--    in safeDiv 14 d
-- :}
-- 2
--
-- >>> :{
--   -- total wrapper over Data.Text's partial splitOn.
--   let safeSplitOn :: Implies p NonEmpty => Refined p Text -> Text -> [Text]
--       safeSplitOn needle haystack = unrefine needle `splitOn` haystack
--       --
--       splitter = $$(refineTH @NonEmpty @Text ",")
--    in safeSplitOn splitter "some,text,"
-- :}
-- ["some","text",""]
--
-- @since 0.1.0.0
type Implies :: Type -> Type -> Constraint
type family Implies q p where
  Implies q p = ErrIfFalse (PredNotFound q p) (ImpliesBool q p)

-- | Infix operator for 'Implies'.
--
-- ==== __Examples__
-- >>> :{
--   safeDiv2 :: (p :=> NonZero, Integral n) => n -> Refined p n -> n
--   safeDiv2 x d = x `div` unrefine d
-- :}
--
-- @since 0.1.0.0
type (:=>) :: Type -> Type -> Constraint

type p :=> q = Implies p q

infixr 1 :=>

-- | @ImpliesBool q p@ returns @'True@ if @q@ logically implies @p@,
-- i.e., whenever @q@ is true, @p@ is also true.
--
-- NB. For a (possibly exclusive) disjunction to imply @p@, /both/ clauses
-- must individually imply @p@, since we cannot know which one is satisfied.
-- That is,
--
-- \[
--   q_1 \vee q_2 \implies p \quad \iff \quad (q_1 \implies p) \wedge (q_2 \implies p)
-- \]
--
-- When determining whether @q@ implies @p@ we are unfortunately limited by
-- searching for /syntactic/, not semantic, equality. In particular,
--
-- @
--   ((A && B) && C) /=> (A && (B && C))
--   (A || (B && Not B) || C) /=> A || C
-- @
--
-- Thus it is advisable to write function constraints as simply as possible.
--
-- @
-- -- not this
-- foo :: Implies p (A && B) => Refined p a -> ...
-- -- prefer this
-- bar :: (Implies p A, Implies p B) => Refined p a -> ...
-- @
--
-- Of course this transformation cannot be performed universally
-- (e.g. @A || B@), and we do have
--
-- @
--   (A && B) == (B && A), (A || B) == (B || A)
-- @
-- for convenience.
--
-- ==== __Examples__
-- >>> :kind! ImpliesBool (Not A) B
-- ImpliesBool (Not A) B :: Bool
-- = 'False
--
-- >>> :kind! ImpliesBool (Not (Not A)) A
-- ImpliesBool (Not (Not A)) A :: Bool
-- = 'True
--
-- >>> :kind! ImpliesBool (A || B) A
-- ImpliesBool (A || B) A :: Bool
-- = 'False
--
-- >>> :kind! ImpliesBool (A || (A && B)) A
-- ImpliesBool (A || (A && B)) A :: Bool
-- = 'True
--
-- @since 0.1.0.0
type ImpliesBool :: Type -> Type -> Bool
type family ImpliesBool q p where
  ImpliesBool p p = 'True
  ImpliesBool q p = ImpliesCNF (ToCNF q) (ToCNF p)

-- | Emits a 'TypeError' when given 'False'. The parameter type is used in the
-- error message.
--
-- @since 0.1.0.0
type ErrIfFalse :: ErrorMessage -> Bool -> Constraint
type family ErrIfFalse p b where
  ErrIfFalse err 'False = TypeError err
  ErrIfFalse _ _ = ()

-- | Type error for a missing, wanted predicate.
--
-- @since 0.1.0.0
type PredNotFound :: Type -> Type -> ErrorMessage

type PredNotFound p q =
  ( 'Text "Desired predicate "
      ':<>: 'ShowType q
      ':<>: 'Text " was not implied by "
      ':<>: 'ShowType p
  )
