{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

The type 'Table' represents the schema for a table in a relational database.
We use it to

* track __table names__ and __column names__ on the type level, and to
* track __column types__ as Haskell types.

More precisely, the schema for a table is represented by the combination
of the types 'Table', '(:.)', and 'Col'.
Every table schema is an instance of the 'IsTable' class.

Example: The following type is the schema for a database table
with name @person@ and two columns, @name@ and @birthyear@,
whose Haskell types are @Text@ and @Int@, respectively.

> type ExampleTable =
>     Table "person"
>         :. Col "name" Text
>         :. Col "birthyear" Int

The type 'Table' does __not contain__ any data.
Instead, use the 'Row' type family to map a table schema
to a type that contains one row worth of data.

Example: The following value is one potential row of a table that matches
the table schema 'ExampleTable'.

> exampleRow :: Row ExampleTable
> exampleRow = ("Ada Lovelace", 1815)

The types in this module are general,
and not specific to any particular SQL implementation.
-}
module Database.Table.Schema
    (
    -- * Table schema
      IsTable (..)
    , getColumnNames
    , Table (..)
    , Col (..)
    , (:.) (..)

    , IsColumnName
    , getColumnName

    -- * Table data: Row
    , Row
    , Only (..)
    ) where

import Prelude

import Data.Foldable
    ( toList
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Sequence
    ( Seq
    , empty
    , (|>)
    )
import Data.Text
    ( Text
    )
import Data.Tuple.Only
    ( Only (..)
    )
import GHC.TypeLits
    ( KnownSymbol
    , Symbol
    , symbolVal
    )

import qualified Data.Text as T

{-------------------------------------------------------------------------------
    Class
-------------------------------------------------------------------------------}
-- | Class of named tables with named columns.
--
-- The data contained in the table is essentially a list of rows
-- with the given column names.
class IsTable t where
    getTableName :: proxy t -> Text
    getColumnNamesSeq :: proxy t -> Seq Text

getColumnNames :: IsTable t => proxy t -> [Text]
getColumnNames = toList . getColumnNamesSeq

{-------------------------------------------------------------------------------
    Type
-------------------------------------------------------------------------------}
-- | Infix notation for a pair of types.
data a :. b = a :. b
    deriving (Eq,Ord,Show,Read)
infixl 3 :.

-- | Named database column.
data Col (name :: Symbol) a = Col
    deriving (Eq,Ord,Show)

-- | Constraint synonym for 'getColName'.
type IsColumnName (name :: Symbol) = KnownSymbol name

-- | Get the name of a column from its type.
getColumnName :: forall name a. IsColumnName name => Col name a -> Text
getColumnName _ = T.pack $ symbolVal (Proxy :: Proxy name)

-- | Named database table.
data Table (name :: Symbol) = Table
    deriving (Eq,Ord,Show)

instance KnownSymbol name => IsTable (Table name) where
    getTableName _ = T.pack $ symbolVal (Proxy :: Proxy name)
    getColumnNamesSeq  _ = empty

instance (IsTable t, KnownSymbol name) => IsTable (t :. Col name a) where
    getTableName _ = getTableName (Proxy :: Proxy t)
    getColumnNamesSeq  _ =
        getColumnNamesSeq (Proxy :: Proxy t)
        |> T.pack (symbolVal (Proxy :: Proxy name))

{-------------------------------------------------------------------------------
    Columns
-------------------------------------------------------------------------------}
-- | Type family
-- that maps a table schema @t@ (which ideally satisfies @IsTable t@)
-- to a type representing rows of that table.
type family Row t

type instance Row (Table n0 :. Col n1 a1) =
    Only a1

type instance Row (Table n0 :. Col n1 a1 :. Col n2 a2) =
    (a1, a2)

type instance Row (Table n0 :. Col n1 a1 :. Col n2 a2 :. Col n3 a3) =
    (a1, a2, a3)

type instance
    Row (Table n0
            :. Col n1 a1
            :. Col n2 a2
            :. Col n3 a3
            :. Col n4 a4
        ) =
        (a1, a2, a3, a4)

type instance
    Row (Table n0
            :. Col n1 a1
            :. Col n2 a2
            :. Col n3 a3
            :. Col n4 a4
            :. Col n5 a5
        ) =
        (a1, a2, a3, a4, a5)

type instance
    Row (Table n0
            :. Col n1 a1
            :. Col n2 a2
            :. Col n3 a3
            :. Col n4 a4
            :. Col n5 a5
            :. Col n6 a6
        ) =
        (a1, a2, a3, a4, a5, a6)
