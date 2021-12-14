module Main (main) where

import Test.DocTest qualified as DT

main :: IO ()
main = DT.doctest args
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