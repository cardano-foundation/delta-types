{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Demo.Database where

import Prelude

import Data.Chain
    ( DeltaChain (..)
    , Edge (..)
    , chainIntoTable
    )
import Data.Generics.Internal.VL
    ( Iso'
    , iso
    , withIso
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Table
    ( DeltaDB (..)
    , Pile (..)
    , tableIntoDatabase
    )
import Data.Text
    ( Text
    )
import Database.Table.Delta
    ( newSqlStore
    )
import Database.Table.Schema
    ( Col (..)
    , Row
    , Table (..)
    , (:.) (..)
    )

import qualified Data.Chain as Chain
import qualified Database.Table.SQLite.Simple as Sql

import Data.DBVar
import Data.Delta
import Data.Store

{-------------------------------------------------------------------------------
    (Mock) address type
-------------------------------------------------------------------------------}
type Address = Text
type Node = Int

data AddressInPool = AddressInPool
    { address :: Address
    , index   :: Int
    } deriving (Eq, Ord, Show)

-- | Construnct an 'Embedding' of delta encodings from an isomorphism.
embedIso :: Iso' a b -> Embedding [DeltaDB Int a] [DeltaDB Int b]
embedIso i = withIso i $ \ab ba -> mkEmbedding Embedding'
    { load = Right . fmap ba
    , write = fmap ab
    , update = \_ _ -> fmap (fmap ab)
    }

type StoreAddress = UpdateStore IO (DeltaChain Node [AddressInPool])

{-------------------------------------------------------------------------------
    Store using SQL row types
-------------------------------------------------------------------------------}

type Primary = Int

newStoreAddressSql :: Sql.Connection -> IO StoreAddress
newStoreAddressSql conn = do
    flip Sql.runSqlM conn $
        Sql.createTable (Proxy :: Proxy (TableAddress :. Col "id" Primary))
    store0 <- newSqlStore proxyTableAddress conn
    embedStore embed store0
  where
    embed = embedIso addressSqlIso
        `o` (tableIntoDatabase `o` chainIntoTable Pile getPile)

addressSqlIso :: Iso' (Edge Node AddressInPool) (Row TableAddress)
addressSqlIso = iso ab ba
  where
    ab Edge{from,to,via=AddressInPool{address,index}} =
        (from, to, address, index)
    ba (from, to, address, index) =
        Edge{from,to,via=AddressInPool{address,index}}

type TableAddress =
    Table "addresses"
        :. Col "from" Node
        :. Col "to" Node
        :. Col "address" Address
        :. Col "address_ix" Int

proxyTableAddress :: Proxy TableAddress
proxyTableAddress = Proxy

{-------------------------------------------------------------------------------
    Database connection
-------------------------------------------------------------------------------}
main :: IO ()
main = Sql.withConnection ":memory:" $ \conn -> do
    store <- newStoreAddressSql conn
    db <- initDBVar store
        $ Chain.fromEdge Edge{from=0,to=1,via=[AddressInPool "a" 31]}

    updateDBVar db $ Chain.AppendTip 2 [AddressInPool "b" 32]
    updateDBVar db $ Chain.AppendTip 3 [AddressInPool "c" 33]
    updateDBVar db $ Chain.CollapseNode 2
    updateDBVar db $ Chain.CollapseNode 1

    print =<< readDBVar db
    print =<< loadS store
    -- FIXME: Results differ, something is wrong.
