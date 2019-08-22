# Changelog


## [0.2.1] - 2019-08-22
- fixed haddock build for ghc 8.6
- fixed numerical instability in gaussian elimination algorithm

## [0.2] - 2019-08-21
- removed singletons from dependencies
- removed eigen from dependencies

- replaced eigen with hmatrix
- added implementation of gaussian elimination based on hmatrix

- added test suite
- integration with travis ci

- stack.yaml and cabal.project contain workaround for type-checker
  error when using haddock with ghc 8.6

- ensured backwards compatibility down to ghc 8.0.2 (stackage lts-9.21)

## [0.1.0.0] - 2019-08-16
Initial release.
