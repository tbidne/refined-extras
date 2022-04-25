<div align="center">

# Refined Extras

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/refined-extras?include_prereleases&sort=semver)](https://github.com/tbidne/refined-extras/releases/)
[![BSD-3-Clause](https://img.shields.io/github/license/tbidne/refined-extras?color=blue)](https://opensource.org/licenses/BSD-3-Clause)

[![nix](https://img.shields.io/github/workflow/status/tbidne/refined-extras/nix/main?label=nix%209.2.2&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/nix_ci.yaml)
[![stack](https://img.shields.io/github/workflow/status/tbidne/refined-extras/stack/main?label=stack%2019.4&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/stack_ci.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/refined-extras/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/style_ci.yaml)

[![8.10.7](https://img.shields.io/github/workflow/status/tbidne/refined-extras/8.10.7/main?label=8.10.7&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/ghc_8-10.yaml)
[![9.0.2](https://img.shields.io/github/workflow/status/tbidne/refined-extras/9.0.2/main?label=9.0.2&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/ghc_9.0.yaml)
[![9.2.1](https://img.shields.io/github/workflow/status/tbidne/refined-extras/9.2.1/main?label=9.2.1&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/ghc_9.2.yaml)

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