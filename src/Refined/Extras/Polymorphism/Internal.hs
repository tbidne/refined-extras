{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for predicate polymorphism. These are all used to implement
-- the 'Refined.Polymorphism.Implies' type family in "Refined.Polymorphism".
--
-- @since 0.1.0.0
module Refined.Extras.Polymorphism.Internal
  ( ImpliesCNF,
    ImpliesCNFHelper,
    ToCNF,
    ToCNFHelper,
    IsCNF,
    Reduce,
    PropEquals,
  )
where

import Data.Kind (Type)
import Data.Type.Bool qualified as B
import Refined (And, Not, Or, Xor, type (&&), type (||))

-- $setup
-- >>> import Data.Type.Equality (type (==))
-- >>> import Refined (NonZero, NonNegative, Positive)

-- | @ImpliesCNF q p@ determines if @q@ implies @p@, i.e., returns 'True'
-- iff whenever @q@ is true, then @p@ is also true. We assume @q@ and @p@
-- are in conjunctive normal form.
--
-- @since 0.1.0.0
type ImpliesCNF :: Type -> Type -> Bool
type family ImpliesCNF q p where
  ImpliesCNF q p = ImpliesCNFHelper (PropEquals q p) q p

-- | Helper for 'ImpliesCNF'.
--
-- Once again split the (equals) condition check from the recursive call
-- so that we do not do any extra work due to strictness.
--
-- @since 0.1.0.0
type ImpliesCNFHelper :: Bool -> Type -> Type -> Bool
type family ImpliesCNFHelper propEq q p where
  ImpliesCNFHelper 'True _ _ = 'True
  ImpliesCNFHelper _ p p = 'True
  ImpliesCNFHelper _ (q && r) p = ImpliesCNF q p B.|| ImpliesCNF r p
  ImpliesCNFHelper _ (q || r) p = ImpliesCNF q p B.&& ImpliesCNF r p
  ImpliesCNFHelper _ q p = 'False

-- | Transforms a predicate expression into conjunctive normal form.
-- This /should/ be total for propositions (obviously other 'Type's will
-- get "stuck"), but a proof of termination would be nice.
--
-- @since 0.1.0.0
type ToCNF :: Type -> Type
type family ToCNF p where
  ToCNF p = ToCNFHelper (IsCNF p) p

-- | Helper for 'ToCNF'. We split up the condition check and reduction step
-- because type families are not guaranteed to be evaluated lazily.
-- In particular, the obvious implementation:
--
-- @
-- B.If (IsCNF p) p (ToCNF (Reduce p))
-- @
--
-- creates an infinite loop.
-- See: https://gitlab.haskell.org/ghc/ghc/-/issues/18965
--
-- @since 0.1.0.0
type ToCNFHelper :: Bool -> Type -> Type
type family ToCNFHelper cond p where
  ToCNFHelper 'True p = p
  ToCNFHelper 'False p = ToCNF (Reduce p)

-- | Returns true if the formula is in conjunctive normal form.
-- See https://en.wikipedia.org/wiki/Conjunctive_normal_form.
--
-- @since 0.1.0.0
type IsCNF :: Type -> Bool
type family IsCNF p where
-- Not p is CNF if p is an atom
  IsCNF (Not (And _ _)) = 'False
  IsCNF (Not (Or _ _)) = 'False
  IsCNF (Not (Xor _ _)) = 'False
  IsCNF (Not (Not _)) = 'False
  IsCNF (Not _) = 'True
-- Or p q is CNF as long as there are no nested Ands and p and q are CNF
  IsCNF (Or (And _ _) _) = 'False
  IsCNF (Or _ (And _ _)) = 'False
  IsCNF (Or p q) = IsCNF p B.&& IsCNF q
-- We want Xor completely eliminated
  IsCNF (Xor _ _) = 'False
-- And p q is CNF if both p and q are CNF
  IsCNF (And p q) = IsCNF p B.&& IsCNF q
  IsCNF _ = 'True

-- | Reduces towards conjunctive normal form. The result is not guaranteed to
-- be in CNF, i.e., 'Reduce' may need to be applied repeatedly.
--
-- @since 0.1.0.0
type Reduce :: Type -> Type
type family Reduce p where
-- Eliminate Xor
  Reduce (Xor p q) = Reduce ((p && q) || (Not p && Not q))
-- Double negation
  Reduce (Not (Not p)) = Reduce p
-- De Morgan's laws
  Reduce (Not (p || q)) = Not (Reduce p) && Not (Reduce q)
  Reduce (Not (p && q)) = Not (Reduce p) && Not (Reduce q)
-- Remaining Not (i.e. Xor)
  Reduce (Not p) = Not (Reduce p)
-- Distributive laws
  Reduce (Or (p && q) r) = And (Reduce p || Reduce r) (Reduce q || Reduce r)
  Reduce (Or r (p && q)) = And (Reduce p || Reduce r) (Reduce q || Reduce r)
-- Remaining Or, And
  Reduce (p || q) = Reduce p || Reduce q
  Reduce (p && q) = Reduce p && Reduce q
-- Base case, nothing to reduce
  Reduce p = p

-- | Equals that ignores order (e.g. (A && B) == (B && A)). Equality is
-- /syntactic/, not semantic. For instance,
--
-- @
-- (A && B) && C /= A && (B && C).
-- @
--
-- ==== __Examples__
-- >>> :kind! PropEquals (NonNegative && (NonZero && NonNegative)) ((NonNegative && NonZero) && NonNegative)
-- PropEquals (NonNegative && (NonZero && NonNegative)) ((NonNegative && NonZero) && NonNegative) :: Bool
-- = 'False
--
-- >>> :kind! PropEquals (And (NonNegative || NonZero) (Not NonZero)) (And (NonNegative || NonZero) (Not NonZero))
-- PropEquals (And (NonNegative || NonZero) (Not NonZero)) (And (NonNegative || NonZero) (Not NonZero)) :: Bool
-- = 'True
--
-- >>> :kind! PropEquals (And NonNegative NonZero) (And NonZero NonZero)
-- PropEquals (And NonNegative NonZero) (And NonZero NonZero) :: Bool
-- = 'False
--
-- >>> :kind! PropEquals (Not (And NonNegative NonZero)) (Not (And NonZero NonNegative))
-- PropEquals (Not (And NonNegative NonZero)) (Not (And NonZero NonNegative)) :: Bool
-- = 'True
--
-- @since 0.1.0.0
type PropEquals :: Type -> Type -> Bool
type family PropEquals p q where
  PropEquals p p = 'True
-- need this in case we have nested And,Or,Xor.
  PropEquals (Not p) (Not q) = PropEquals p q
-- check reverse order for these 3
  PropEquals (And p q) (And q p) = 'True
  PropEquals (Or p q) (Or q p) = 'True
  PropEquals (Xor p q) (Xor q p) = 'True
  PropEquals p q = 'False
