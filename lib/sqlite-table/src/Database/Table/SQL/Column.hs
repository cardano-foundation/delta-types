{-# LANGUAGE FlexibleInstances #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

SQL column types.
-}
module Database.Table.SQL.Column
    ( SqlType
    , escapeSqlType
    , IsColumnSql (..)
    ) where

import Prelude

import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )

import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite.Simple.ToField as Sqlite

{-------------------------------------------------------------------------------
    Types for database columns
-------------------------------------------------------------------------------}
-- | SQL column types, including constraints.
--
-- Internally represented as a textual string. Examples:
--
-- > INTEGER  PRIMARY KEY NOT NULL
-- > TEXT     NOT NULL
newtype SqlType = SqlType Text
    deriving (Eq,Ord,Show)

-- | Print an 'SqlType' in SQL syntax.
escapeSqlType :: SqlType -> Text
escapeSqlType (SqlType x) = x

-- | Class that maps a Haskell type to a column type of an SQL database.
-- Also ensures that the types can be converted to each other
-- (via 'Sqlite.ToField' and 'Sqlite.FromField'
-- from "Database.SQLite.Simple").
class (Sqlite.ToField a, Sqlite.FromField a) => IsColumnSql a where
    getSqlType :: proxy a -> SqlType

instance IsColumnSql Int where
    getSqlType _ = SqlType "INTEGER NOT NULL"

instance IsColumnSql (Maybe Int) where
    getSqlType _ = SqlType "INTEGER"

instance IsColumnSql Text where
    getSqlType _ = SqlType "TEXT NOT NULL"

instance IsColumnSql (Maybe Text) where
    getSqlType _ = SqlType "TEXT"

instance IsColumnSql String where
    getSqlType _ = SqlType "TEXT NOT NULL"

instance IsColumnSql (Maybe String) where
    getSqlType _ = SqlType "TEXT"

instance IsColumnSql ByteString where
    getSqlType _ = SqlType "BLOB NOT NULL"

instance IsColumnSql (Maybe ByteString) where
    getSqlType _ = SqlType "BLOB"
