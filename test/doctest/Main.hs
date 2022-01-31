module Main (main) where

import System.Environment qualified as Env
import Test.DocTest qualified as DT

main :: IO ()
main = do
  shouldRun <- Env.lookupEnv "RUN_DOCTEST"
  case shouldRun of
    Just "true" -> DT.doctest args
    _ -> putStrLn "*** Doctests Disabled ***"
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Refined/Extras/Polymorphism.hs",
    "src/Refined/Extras/Polymorphism/Internal.hs",
    "src/Refined/Extras/Predicates/Foldable.hs",
    "src/Refined/Extras/Predicates/Text.hs",
    "src/Refined/Extras/Unsafe.hs",
    "src/Refined/Extras/Utils.hs"
  ]

exts :: [String]
exts =
  [ "-XConstraintKinds",
    "-XDataKinds",
    "-XDeriveGeneric",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XImportQualifiedPost",
    "-XMultiParamTypeClasses",
    "-XOverloadedStrings",
    "-XPatternSynonyms",
    "-XScopedTypeVariables",
    "-XStandaloneKindSignatures",
    "-XTypeApplications",
    "-XTypeFamilies",
    "-XTypeOperators"
  ]
