{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

'Table' types that can be mapped to SQL tables.
-}
module Database.Table.SQL.Table
    ( IsTableSql
    , getColumnSqlTypes
    , HasColumnsSql (..)
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
import Database.Table.Schema
    ( Col
    , IsTable
    , Row
    , Table
    , (:.)
    )
import Database.Table.SQL.Column
    ( IsColumnSql (getSqlType)
    , SqlType
    )

import qualified Database.SQLite.Simple as Sqlite

{-------------------------------------------------------------------------------
    Types for database tables
-------------------------------------------------------------------------------}

{- |
Constaint for types that represent tables that can be mapped to SQL tables.

In particular, the type @t@ tracks table names and column names
(via 'IsTable'),
and has column types that can be converted to and from SQL types
(via 'HasColumns', 'Sqlite.ToRow' and 'Sqlite.FromRow').

Note: When using this constraint synonym, you will have to enable
the @FlexibleContexts@ extension.

Typically, types constructed using 'Database.Table.Schema.Table'
from "Database.Table.Schema" are automatically instances of 'IsTableSql',
as long as their column types can be converted to and from SQL types.
For example, the following type is automatically an instance:

@
type ExampleTable =
    Table "person"
        :. Col "name" Text
        :. Col "birthyear" Int
@
-}
type IsTableSql t =
    ( IsTable t
    , HasColumnsSql t
    , Sqlite.ToRow (Row t)
    , Sqlite.FromRow (Row t)
    )

-- | Get column types as a plain list.
getColumnSqlTypes :: IsTableSql t => proxy t -> [SqlType]
getColumnSqlTypes = toList . getColumnSqlTypesSeq

-- | Class for types that correspond to a sequence (list) of SQL types.
class HasColumnsSql t where
    getColumnSqlTypesSeq :: proxy t -> Seq SqlType

instance HasColumnsSql (Table name) where
    getColumnSqlTypesSeq _ = empty

instance (HasColumnsSql t, IsColumnSql a)
    => HasColumnsSql (t :. Col name a)
  where
    getColumnSqlTypesSeq _ =
        getColumnSqlTypesSeq (Proxy :: Proxy t)
            |> getSqlType (Proxy :: Proxy a)
