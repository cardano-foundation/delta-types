{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Storing 'Database.Table.Schema.Table' in SQLite databases.

This module is meant to be imported qualified, i.e.

@
import qualified Database.Table.SQLite.Simple as Sql
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
In other words, knowledge of SQL is not needed to use this module,
but can be helpful.
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

Here an example of how to use this module.

We begin by importing a helper module

> import Data.Proxy ( Proxy (..) )

and the modules relating to databases:

> import Database.Table
> import qualified Database.Table.SQLite.Simple as Sql

Next, we define a type for the table that we want to store in the database:

> type TablePerson =
>     Table "person"
>         :. Col "name" Text
>         :. Col "birthyear" Int

The above type defines a table of name @person@
with two columns @name@ and @birthyear@.
The type is automatically an @instance@ of 'IsTableSql'.

Here is an example row that we want to insert into the table:

> exampleRow :: Row TablePerson
> exampleRow = ("Ada Lovelace", 1815)

Next, we define two conveniences that bring the type level
to the value level:

> tablePerson :: Proxy TablePerson
> tablePerson = Proxy
>
> colBirthYear :: Col "birthyear" Int
> colBirthYear = Col

Then, we define a sequence of operations that we want to perform
on the database:
First, we create the table,
then we insert two rows,
then, we ask for the number of rows that satisfy a certain condition:

> example :: Sql.SqlM Int
> example = do
>     Sql.createTable tablePerson
>     Sql.insertOne ("Babbage", 1791) tablePerson
>     Sql.insertOne exampleRow tablePerson
>     length <$> Sql.selectWhere (colBirthYear Sql.>. 1800) tablePerson

In the definition above, the type checker makes sure that

* the inserted rows are valid for the table, and that
* the value in the condition is valid for the column.

However, the type checker does not check that the condition
mentions only columns that occur in the table.

Finally, we create an in-memory database, run the above operations
and print the result. This result should be equal to @1@,
as the 'selectWhere' operation should return exactly one row
in our example.

> main :: IO ()
> main = do
>     rowCount <- Sql.withConnection ":memory:"
>         $ Sql.runSqlM example
>     print rowCount -- should be equal to 1

-}
