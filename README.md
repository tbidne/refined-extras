<div align="center">

# Refined Extras

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/refined-extras?include_prereleases&sort=semver)](https://github.com/tbidne/refined-extras/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/refined-extras/ci.yaml?branch=main)](https://github.com/tbidne/refined-extras/actions/workflows/ci.yaml)
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