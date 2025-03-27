{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Copyright   : © 2022-2023 IOHK, 2023-2025 Cardano Foundation
License     : Apache-2.0
Description : Manipulate an SQL database table using delta types.

This module provides a 'Store' for tables
using "Database.Table.SQLite.Simple".
-}
module Database.Table.Delta
    ( newSqlStore
    ) where

import Prelude

import Control.Monad
    ( forM_
    , void
    )
import Control.Monad.Class.MonadThrow
    ( throwIO
    )
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Data.Delta
    ( Delta (..)
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Store
    ( UpdateStore
    , hoistStore
    , mkUpdateStore
    , updateLoad
    )
import Data.Table
    ( DeltaDB (..)
    , Pile (..)
    , Table (..)
    )
import Database.Table.Schema
    ( Col (..)
    , IsColumnName
    , Row
    , (:.) (..)
    )
import Database.Table.SQLite.Simple
    ( IsColumnSql
    )
import Control.Monad.Class.MonadThrow
    ( MonadThrow
    )
import Data.IORef
    ( newIORef
    , readIORef
    , writeIORef
    )

import qualified Data.Table as Table
import qualified Database.Table.Schema as Sql
import qualified Database.Table.SQLite.Simple as Sql

{-------------------------------------------------------------------------------
    Database operations
-------------------------------------------------------------------------------}
-- | Helper abstraction for a Database backend
data Database m key row = Database
    { selectAll   :: m [(key, row)]
    , deleteAll   :: m ()
    , insertMany  :: [(key, row)] -> m ()
    , deleteOne   :: key -> m ()
    , updateOne   :: (key, row) -> m ()
    }

-- | Helper type for primary keys
type Primary = Int

-- | SQL database backend
sqlDB
    :: forall table.
        ( AppendPrimaryKey table
        , Sql.IsTableSql (table :. Col "id" Primary)
        )
    => Proxy table -> Database Sql.SqlM Int (Row table)
sqlDB _ = Database
    { selectAll = map (fromKeyedRow proxy0) <$> Sql.selectAll proxy1
    , deleteAll = Sql.deleteAll proxy1
    , insertMany = \zs ->
        Sql.insertMany (map (toKeyedRow proxy0) zs) proxy1
    , deleteOne = \key ->
        Sql.deleteWhere (colKey Sql.==. key) proxy1
    , updateOne = \(key, row) ->
        Sql.updateWhere (colKey Sql.==. key) (mkRowUpdate proxy0 row) proxy1
    }
  where
    proxy0 = Proxy :: Proxy table
    proxy1 = Proxy :: Proxy (table :. Col "id" Primary)
    colKey = Col :: Col "id" Primary

class AppendPrimaryKey t where
    toKeyedRow :: Proxy t -> (Primary, Row t) -> Row (t :. Col "id" Primary)
    fromKeyedRow :: Proxy t -> Row (t :. Col "id" Primary) -> (Primary, Row t)
    mkRowUpdate :: Proxy t -> Row t -> [Sql.Update]

instance
    ( IsColumnName n1
    , IsColumnSql a1
    )
    => AppendPrimaryKey (Sql.Table n0 :. Col n1 a1)
  where
    toKeyedRow _ (key, Sql.Only x1) = (x1, key)
    fromKeyedRow _ (x1, key) = (key, Sql.Only x1)
    mkRowUpdate _ (Sql.Only x1) = [ (Col :: Col n1 a) Sql.=. x1 ]

instance
    ( IsColumnName n1, IsColumnName n2
    , IsColumnSql a1, IsColumnSql a2
    )
    => AppendPrimaryKey (Sql.Table n0 :. Col n1 a1 :. Col n2 a2)
  where
    toKeyedRow _ (key, (x1, x2)) = (x1, x2, key)
    fromKeyedRow _ (x1, x2, key) = (key, (x1, x2))
    mkRowUpdate _ (x1, x2) =
        [ (Col :: Col n1 a1) Sql.=. x1
        , (Col :: Col n2 a2) Sql.=. x2
        ]

instance
    ( IsColumnName n1, IsColumnName n2, IsColumnName n3
    , IsColumnSql a1, IsColumnSql a2, IsColumnSql a3
    )
    => AppendPrimaryKey (Sql.Table n0 :. Col n1 a1 :. Col n2 a2 :. Col n3 a3)
  where
    toKeyedRow _ (key, (x1, x2, x3)) = (x1, x2, x3, key)
    fromKeyedRow _ (x1, x2, x3, key) = (key, (x1, x2, x3))
    mkRowUpdate _ (x1, x2, x3) =
        [ (Col :: Col n1 a1) Sql.=. x1
        , (Col :: Col n2 a2) Sql.=. x2
        , (Col :: Col n3 a3) Sql.=. x3
        ]

instance
    ( IsColumnName n1, IsColumnName n2, IsColumnName n3, IsColumnName n4
    , IsColumnSql a1, IsColumnSql a2, IsColumnSql a3, IsColumnSql a4
    )
    => AppendPrimaryKey (Sql.Table n0 :. Col n1 a1 :. Col n2 a2 :. Col n3 a3 :. Col n4 a4)
  where
    toKeyedRow _ (key, (x1, x2, x3, x4)) = (x1, x2, x3, x4, key)
    fromKeyedRow _ (x1, x2, x3, x4, key) = (key, (x1, x2, x3, x4))
    mkRowUpdate _ (x1, x2, x3, x4) =
        [ (Col :: Col n1 a1) Sql.=. x1
        , (Col :: Col n2 a2) Sql.=. x2
        , (Col :: Col n3 a3) Sql.=. x3
        , (Col :: Col n4 a4) Sql.=. x4
        ]

{-------------------------------------------------------------------------------
    Database operations
-------------------------------------------------------------------------------}

-- | Construct a 'UpdateStore' from an SQL table.
--
-- The unique IDs will be stored in a column "id" at the end of
-- each row in the database table.
newSqlStore
    :: forall table.
       ( AppendPrimaryKey table
       , Sql.IsTableSql (table :. Col "id" Primary)
       , Show (Row table)
       )
    => Proxy table
    -> Sql.Connection
    -> IO (UpdateStore IO [DeltaDB Int (Row table)])
newSqlStore proxy conn = do
    store' <- newDatabaseStore proxy (sqlDB (Proxy :: Proxy table))
    pure $ hoistStore (\m -> Sql.runSqlM m conn) store'

-- | Helper function to create a 'UpdateStore' using a 'Database' backend.
newDatabaseStore
    :: forall n table. (MonadIO n, Show (Row table))
    => Proxy table
    -> Database Sql.SqlM Int (Row table)
    -> n (UpdateStore Sql.SqlM [DeltaDB Int (Row table)])
newDatabaseStore _ db = do
    ref <- liftIO $ newIORef Nothing
    let rememberSupply table = writeIORef ref $ Just $ uids table
        load = do
            -- read database table, preserve keys
            table <- Table.fromRows <$> selectAll db
            -- but use our own unique ID supply
            liftIOSqlM (readIORef ref) >>= \case
                Just supply  -> pure $ Right table{uids = supply}
                Nothing      -> do
                    liftIOSqlM $ rememberSupply table
                    pure $ Right table
        write table = void $ do
            deleteAll db -- delete any old data in the table first
            insertMany db $ getPile $ Table.toRows table
            liftIOSqlM $ rememberSupply table
        update = updateLoad load throwIO $ \table ds -> do
            mapM_ (update1 table) ds
            liftIOSqlM $ rememberSupply (apply ds table) -- need to use updated supply
    pure $ mkUpdateStore load write update
  where
    update1 _ (InsertManyDB zs) = void $ insertMany db zs
    update1 _ (DeleteManyDB ks) = forM_ ks $ deleteOne db
    update1 _ (UpdateManyDB zs) = forM_ zs $ updateOne db

-- TODO: This is cheating.
-- Think more carefully what effects `SqlM` should support.
-- I think that we do want to support global state as `IORef`,
-- that we can share with `IO`,
-- but I'm not sure if that is going to happen.
liftIOSqlM :: IO a -> Sql.SqlM a
liftIOSqlM action = Sql.rawSqlite (\_ -> action)

{- Note [Unique ID supply in newDBStore]

We expect that updating the store and loading the value
is the same as first loading the value and then apply the delta,
i.e. we expect that the two actions

    loadS >>= \a -> updateS a da >>= loadS
    loadS >>= \a -> pure $ apply da a

are operationally equivalent.
However, this is only the case if we keep track of the supply
of unique IDs for the table! Otherwise, loading the table
from the database again can mess up the supply.
-}
-- TODO: For clarity, we may want to implement this in terms
-- of a product of stores ("semidirect product").
