# doctest-discover-hpack

Automatically discovers and runs
[Doctest](https://hackage.haskell.org/package/doctest)s, based on the current
[hpack](https://hackage.haskell.org/package/hpack) configuration.

### Usage

Define one (or more) test suite(s) in your `package.yaml` configuration:

```yaml
tests:
  my-package-test:
    main:                Doctest.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - doctest
    - doctest-discover-hpack
    - my-package
    - my-package-exe
```

Initialize the corresponding driver file(s) with:
```haskell
{-# OPTIONS_GHC -F -pgmF doctest-discover-hpack #-}
```

Upon running your tests, using eg:

```
stack test my-package-test
```

the dependencies of the test suite(s) will automatically be tested, using any necessary 
language extension, based on:
- the global `default-extensions`,
- the `default-extensions` of the test suite,
- the `default-extensions` of the tested dependency,
- the `{-# LANGUAGE #-}` pragmas of each tested source file.