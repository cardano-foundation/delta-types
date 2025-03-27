## Overview

This package provides type-level SQL tables.

Compatible with [sqlite-simple][].

  [sqlite-simple]: https://hackage.haskell.org/package/sqlite-simple

## Roadmap

* Flip order of arguments for `selectWhere` etc — `proxy t` first.

* The use of `Row` for data is not structually polymorphic enough
  — I frequently had to create new type classes in order to express e.g.
  the use case where I add a new column to an existing table,
  such as a primary key column `table :. Col "id" Primary`.
