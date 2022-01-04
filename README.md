<div align="center">

# Refined Extras

[![Hackage](https://img.shields.io/hackage/v/refined-extras)](https://hackage.haskell.org/package/refined-extras)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/refined-extras)](http://packdeps.haskellers.com/reverse/refined-extras)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/refined-extrasls/badge)](https://matrix.hackage.haskell.org/#/package/refined-extras)
[![Cabal CI](https://img.shields.io/github/workflow/status/tbidne/refined-extras/cabal/main?label=cabal&logoColor=white)](https://github.com/tbidne/refined-extras/actions/workflows/cabal_ci.yaml)
[![Stack CI](https://img.shields.io/github/workflow/status/tbidne/refined-extras/stack/main?label=stack&logoColor=white)](https://github.com/tbidne/refined-extras/actions/workflows/stack_ci.yaml)
[![Nix CI](https://img.shields.io/github/workflow/status/tbidne/refined-extras/nix/main?label=nix&logo=nixos&logoColor=white)](https://github.com/tbidne/refined-extras/actions/workflows/nix_ci.yaml)
[![BSD-3-Clause](https://img.shields.io/github/license/tbidne/refined-extras?color=blue)](https://opensource.org/licenses/BSD-3-Clause)
</div>

# Description

`refined-extras` provides extra functionality for the [refined](https://hackage.haskell.org/package/refined) package. This functionality can be broken into several categories.

## Polymorphism

Allows us to write functions that are polymorphic in the predicate constraints they require, e.g.,

```haskell
safeDiv :: Implies p NonZero => Int -> Refined p Int -> Int
```

## Predicates

Predefined predicates.

## Unsafe

Unsafe functions for when we __know__ something holds but cannot prove it to the type system, e.g.,

```haskell
let m = $$(refineTH 7) :: Refined Positive Int
    n = $$(refineTH 8) :: Refined Positive Int
 in unsafeLiftR2 (+) m n -- Refined Positive Int
```

## Utils

Various convenience utilities.

The entrypoint is `Refined.Extras`, which reexports everything.