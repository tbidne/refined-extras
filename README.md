<div align="center">

# Refined Extras

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/refined-extras?include_prereleases&sort=semver)](https://github.com/tbidne/refined-extras/releases/)
[![BSD-3-Clause](https://img.shields.io/github/license/tbidne/refined-extras?color=blue)](https://opensource.org/licenses/BSD-3-Clause)

[![nix](https://img.shields.io/github/workflow/status/tbidne/refined-extras/nix/main?label=nix%209.2&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/nix_ci.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/refined-extras/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/style_ci.yaml)

[![cabal 8.10](https://img.shields.io/github/workflow/status/tbidne/refined-extras/cabal_8-10/main?label=8.10&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/cabal_8-10.yaml)
[![cabal 9.0](https://img.shields.io/github/workflow/status/tbidne/refined-extras/cabal_9-0/main?label=9.0&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/cabal_9-0.yaml)
[![cabal 9.2](https://img.shields.io/github/workflow/status/tbidne/refined-extras/cabal_9-2/main?label=9.2&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/cabal_9-2.yaml)

[![stack lts-18](https://img.shields.io/github/workflow/status/tbidne/refined-extras/stack_lts-18/main?label=stack%20lts-18&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/stack_lts-18.yaml)
[![stack lts-19](https://img.shields.io/github/workflow/status/tbidne/refined-extras/stack_lts-19/main?label=stack%20lts-19&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/stack_lts-19.yaml)
[![stack nightly](https://img.shields.io/github/workflow/status/tbidne/refined-extras/stack_nightly/main?label=stack%20nightly&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/refined-extras/actions/workflows/stack_nightly.yaml)

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