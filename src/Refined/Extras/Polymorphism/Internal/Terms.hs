-- | This module provides an implementation of the
-- 'Refined.Extras.Polymorphism.ImpliesBool' type family at the term
-- level, to facilitate testing the algorithm.
--
-- @since 0.1.0.0
module Refined.Extras.Polymorphism.Internal.Terms
  ( Calculus (..),
    impliesBool,
    toCNF,
    isCNF,
  )
where

-- | @since 0.1.0.0
data Calculus a
  = CAtom a
  | CNot !(Calculus a)
  | CAnd !(Calculus a) !(Calculus a)
  | COr !(Calculus a) !(Calculus a)
  | CXor !(Calculus a) !(Calculus a)
  deriving (Show)

-- | @since 0.1.0.0
instance Eq a => Eq (Calculus a) where
  CAtom x == CAtom y = x == y
  CNot x == CNot y = x == y
  CAnd x1 x2 == CAnd y1 y2 = (x1 == y1) && (x2 == y2) || (x1 == y2) && (x2 == y1)
  COr x1 x2 == COr y1 y2 = (x1 == y1) && (x2 == y2) || (x1 == y2) && (x2 == y1)
  CXor x1 x2 == CXor y1 y2 = (x1 == y1) && (x2 == y2) || (x1 == y2) && (x2 == y1)
  _ == _ = False

infixr 3 `CAnd`

infixr 2 `COr`

infixr 2 `CXor`

-- | @since 0.1.0.0
impliesBool :: Eq a => Calculus a -> Calculus a -> Bool
impliesBool q p
  | q == p = True
  | otherwise = impliesCNF (toCNF q) (toCNF p)

-- | @since 0.1.0.0
impliesCNF :: Eq a => Calculus a -> Calculus a -> Bool
impliesCNF q p = impliesCNFHelper (q == p) q p

-- | @since 0.1.0.0
impliesCNFHelper :: Eq a => Bool -> Calculus a -> Calculus a -> Bool
impliesCNFHelper True _ _ = True
impliesCNFHelper _ q p | q == p = True
impliesCNFHelper _ (q `CAnd` r) p = impliesCNF q p || impliesCNF r p
impliesCNFHelper _ (q `COr` r) p = impliesCNF q p && impliesCNF r p
impliesCNFHelper _ _ _ = False

-- | @since 0.1.0.0
toCNF :: Calculus a -> Calculus a
toCNF p = toCNFHelper (isCNF p) p

-- | @since 0.1.0.0
toCNFHelper :: Bool -> Calculus a -> Calculus a
toCNFHelper True p = p
toCNFHelper False p = toCNF (reduce p)

-- | @since 0.1.0.0
isCNF :: Calculus a -> Bool
isCNF (CNot (CAnd _ _)) = False
isCNF (CNot (COr _ _)) = False
isCNF (CNot (CXor _ _)) = False
isCNF (CNot (CNot _)) = False
isCNF (CNot _) = True
isCNF (COr (CAnd _ _) _) = False
isCNF (COr _ (CAnd _ _)) = False
isCNF (COr p q) = isCNF p && isCNF q
isCNF (CXor _ _) = False
isCNF (CAnd p q) = isCNF p && isCNF q
isCNF _ = True

-- | @since 0.1.0.0
reduce :: Calculus a -> Calculus a
-- Eliminate CXor
reduce (CXor p q) = (p `COr` q) `CAnd` (CNot p `COr` CNot q)
-- Double negation
reduce (CNot (CNot p)) = p
-- De Morgan's laws
reduce (CNot (COr p q)) = CNot p `CAnd` CNot q
reduce (CNot (CAnd p q)) = CNot p `COr` CNot q
-- Remaining CNot (i.e. CXor)
reduce (CNot p) = CNot (reduce p)
-- Distributive laws
reduce (COr (p `CAnd` q) r) = CAnd (p `COr` r) (q `COr` r)
reduce (COr r (p `CAnd` q)) = CAnd (r `COr` p) (r `COr` q)
-- Remaining COr, CAnd
reduce (p `COr` q) = reduce p `COr` reduce q
reduce (p `CAnd` q) = reduce p `CAnd` reduce q
-- Base case, CNothing to reduce
reduce p = p
