-- | Exports numeric generators.
--
-- @since 0.1.0.0
module Gens.Numeric
  ( genPositives,
    genPositivesWithZero,
  )
where

import GHC.Natural (Natural)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR

genPositives :: MonadGen m => m [Natural]
genPositives = genWithLowerBound 1

genPositivesWithZero :: MonadGen m => m [Natural]
genPositivesWithZero = do
  positives <- genWithLowerBound 1
  HG.shuffle $ 0 : positives

genWithLowerBound :: MonadGen m => Int -> m [Natural]
genWithLowerBound lower = HG.list listSz genPositive
  where
    genPositive = fromIntegral <$> HG.int (HR.exponential lower 10_000)
    listSz = HR.constant 0 20
