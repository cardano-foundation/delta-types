## Overview

This repository contains Haskell libraries related to delta types.

For an introduction, watch

1. Apfelmus, H. (2023). [Delta encodings help separate business logic from database operations][bobkonf]. Bobkonf 2023, Berlin.

  [bobkonf]: https://bobkonf.de/2023/apfelmus.html

## Contents

This repository is structured as follows:

* `lib/`
    * `delta-types/` — Basic concepts related to delta types.
    * `delta-store/` — Storing data on disk using delta types.

## QuickStart

Prerequisites:

* [Haskell toolchain](https://www.haskell.org/downloads/) with `cabal` and `ghc`.

To build the packages, use

```console
cabal build all
```

To run the tests for the packages, use

```console
cabal test all
```

## Contributors

See [CONTRIBUTING.md](CONTRIBUTING.md).

## Roadmap

* Finalize and release packages on Hackage

  [hackage]: https://hackage.haskell.org
  [haskell]: https://haskell.org
