-- | Entrypoint for property tests.
--
-- @since 0.1.0.0
module Main (main) where

import Test.Tasty qualified as Tasty
import Tests.Polymorphism.CNF qualified
import Tests.Polymorphism.Implies qualified
import Tests.Predicates.Foldable qualified
import Tests.Predicates.Text qualified

-- | Runs property tests.
--
-- @since 0.1.0.0
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Property tests"
      [ Tests.Polymorphism.CNF.props,
        Tests.Polymorphism.Implies.props,
        Tests.Predicates.Foldable.props,
        Tests.Predicates.Text.props
      ]
