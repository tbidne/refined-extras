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
    "src/Refined/Predicates/Foldable.hs",
    "src/Refined/Predicates/Text.hs",
    "src/Refined/Utils.hs"
  ]

exts :: [String]
exts =
  [ "-XDataKinds",
    "-XDeriveGeneric",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XImportQualifiedPost",
    "-XMultiParamTypeClasses",
    "-XOverloadedStrings",
    "-XScopedTypeVariables",
    "-XStandaloneKindSignatures",
    "-XTypeApplications"
  ]
