{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Storing 'Database.Table.Schema.Table' in SQLite databases.

This module is meant to be imported qualified, i.e.

@
import qualified Database.Table.SQLite.Simple as Sqlite
@

This module provides a basic, self-contained set of functionalities
for reading from and writing to SQLite databases. Specifically, we can

* Convert between Haskell types and SQL types (via "Database.SQLite.Simple").
* Construct expressions, mainly for filter conditions.
* Execute statements, such as retrieving and updating rows.
* Open and close database files.

This module uses small embedded domain specific languages (eDSLs)
to represent database operations
— you do __not need to write SQL__ source code.
In other words, knowledge of SQL is not necessary to use this module.
-}
module Database.Table.SQLite.Simple
    (
    -- * Example

    -- $doc

    -- * Convert between Haskell types and SQL types
      IsTableSql
    , HasColumnsSql (..)
    , getColumnSqlTypes

    , SqlType
    , escapeSqlType
    , IsColumnSql (..)

    -- * SQL Expressions
    , Expr
    , true
    , false
    , not_
    , (&&.)
    , and_
    , (||.)
    , or_
    , (==.)
    , (/=.)
    , (<.)
    , (<=.)
    , (>.)
    , (>=.)

    -- * SQL Statements
    , SqlM
    , SqlException
    , createTable
    , selectAll
    , selectWhere
    , insertOne
    , insertMany
    , deleteAll
    , deleteWhere
    , updateWhere
    , Update
    , (=.)

    -- * Open, run 'SqlM', and close databases
    , Connection
    , open
    , close
    , withConnection
    , runSqlM
    , rawSqlite
    ) where

import Database.Table.SQL.Column
import Database.Table.SQL.Expr
import Database.Table.SQL.Table
import Database.Table.SQLite.Simple.Exec
import Database.Table.SQLite.Simple.Monad

{- $doc

TODO: Brief usage example here.

-}
