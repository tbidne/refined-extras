-- | Exports unsafe functions.
--
-- @since 0.1.0.0
module Refined.Extras.Unsafe
  ( unsafeLiftR,
    unsafeLiftR2,
    unsafeLiftR3,
  )
where

import Refined (Predicate, Refined)
import Refined qualified as R
import Refined.Extras.Utils (pattern MkRefined)
import Refined.Unsafe qualified as RUnsafe

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Refined (Positive, SizeEqualTo, refineTH)

-- | Lifts a function onto 'Refined', dying with an error if the
-- predicate is not preserved. Intended for when we __know__ a function
-- preserves the refinement (e.g. addition of positive integers).
--
-- __WARNING: This function is not total. Exercise restraint!__
--
-- ==== __Examples__
-- >>> unsafeLiftR (fmap even) $$(refineTH @(SizeEqualTo 4) [2, 4, 1, 8])
-- Refined [True,True,False,True]
--
-- @since 0.1.0.0
unsafeLiftR :: Predicate p b => (a -> b) -> Refined p a -> Refined p b
unsafeLiftR f = RUnsafe.unsafeRefine . f . R.unrefine

-- | Lifts a binary function onto 'Refined', dying with an error if the
-- predicate is not preserved. Intended for when we __know__ a function
-- preserves the refinement (e.g. addition of positive integers).
--
-- __WARNING: This function is not total. Exercise restraint!__
--
-- ==== __Examples__
-- >>> unsafeLiftR2 (+) $$(refineTH @Positive 7) $$(refineTH @Positive 11)
-- Refined 18
--
-- @since 0.1.0.0
unsafeLiftR2 ::
  Predicate p c =>
  (a -> b -> c) ->
  Refined p a ->
  Refined p b ->
  Refined p c
unsafeLiftR2 f (MkRefined x) = RUnsafe.unsafeRefine . f x . R.unrefine

-- | Lifts a ternary function onto 'Refined', dying with an error if the
-- predicate is not preserved. Intended for when we __know__ a function
-- preserves the refinement (e.g. addition of positive integers).
--
-- __WARNING: This function is not total. Exercise restraint!__
--
-- ==== __Examples__
-- >>> unsafeLiftR3 (\x y z -> x + y + z) $$(refineTH @Positive 1) $$(refineTH @Positive 2) $$(refineTH @Positive 3)
-- Refined 6
--
-- @since 0.1.0.0
unsafeLiftR3 ::
  Predicate p d =>
  (a -> b -> c -> d) ->
  Refined p a ->
  Refined p b ->
  Refined p c ->
  Refined p d
unsafeLiftR3 f (MkRefined x) (MkRefined y) =
  RUnsafe.unsafeRefine . f x y . R.unrefine
