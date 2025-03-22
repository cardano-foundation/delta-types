{-# OPTIONS_GHC -Wno-redundant-constraints#-}
-- We intentionally specify more constraints than necessary for some exports.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Copyright   : © 2022-2023 IOHK, 2023-2025 Cardano Foundation
License     : Apache-2.0
Description : Mutable variable that mirrors its value in a 'Data.Store.Store'.

'DBVar' represents a mutable variable that stores one Haskell value
in volatile memory (RAM),
but mirrors the value to persistent storage,
for example to a database on disk.

* 'Store' represents the storage facility to which the variable is mirrored.
* Read-access is from volatile memory and highly concurrent.
* Updates are incremental and use delta types, see "Data.Delta".
-}
module Data.DBVar (
      DBVar
    , readDBVar
    , updateDBVar
    , modifyDBVar
    , modifyDBMaybe
    , initDBVar
    , loadDBVar
    ) where

import Prelude

import Control.Concurrent.Class.MonadSTM
    ( MonadSTM
    , atomically
    , newTVarIO
    , readTVar
    , readTVarIO
    , retry
    , writeTVar
    )
import Control.Monad.Class.MonadThrow
    ( MonadEvaluate
    , MonadMask
    , MonadThrow
    , bracket
    , evaluate
    , mask
    , throwIO
    )
import Data.Delta
    ( Delta (..)
    )
import Data.Store
    ( Store (..)
    , UpdateStore
    )

{-------------------------------------------------------------------------------
    DBVar
-------------------------------------------------------------------------------}
-- | A 'DBVar'@ m delta@ is a mutable reference to a Haskell value of type @a@.
-- The type @delta@ is a delta type for this value type @a@,
-- that is we have @a ~ @'Base'@ delta@.
--
-- The Haskell value is cached in memory, in weak head normal form (WHNF).
-- However, whenever the value is updated, a copy of the value will be written
-- to persistent storage like a file or database on disk;
-- the specific storage facility is represented by a 'Store'.
-- For efficient updates, the delta type @delta@ is used in the update.
--
-- Concurrency: 'DBVar' fully supports concurrent reads and updates.
--
-- * Updates are atomic and will block other updates.
-- * Reads will /not/ be blocked during an update
--   (except for a small moment where the new value atomically
--    replaces the old one).
data DBVar m delta = DBVar
    { readDBVar_     :: m (Base delta)
    , modifyDBMaybe_ :: forall b. (Base delta -> (Maybe delta, b)) -> m b
    }

-- | Read the current value of the 'DBVar'.
readDBVar :: (Delta da, a ~ Base da) => DBVar m da -> m a
readDBVar = readDBVar_

-- | Update the value of the 'DBVar' using a delta type.
--
-- The new value will be evaluated to weak head normal form.
updateDBVar :: (Delta da, Monad m) => DBVar m da -> da -> m ()
updateDBVar var delta = modifyDBMaybe var $ \_ -> (Just delta,())

-- | Modify the value in a 'DBVar'.
--
-- The new value will be evaluated to weak head normal form (WHNF).
modifyDBVar
    :: (Delta da, Monad m, a ~ Base da)
    => DBVar m da -> (a -> (da, b)) -> m b
modifyDBVar var f = modifyDBMaybe var $ \a -> let (da,b) = f a in (Just da,b)

-- | Maybe modify the value in a 'DBVar'
--
-- If updated,
-- the new value will be evaluated to weak head normal form (WHNF).
modifyDBMaybe
    :: (Delta da, Monad m, a ~ Base da)
    => DBVar m da -> forall b. (a -> (Maybe da, b)) -> m b
modifyDBMaybe = modifyDBMaybe_

-- | Initialize a new 'DBVar' that mirrors to a given 'Store'.
initDBVar
    ::  ( MonadSTM m, MonadThrow m, MonadEvaluate m, MonadMask m
        , Delta da, a ~ Base da
        )
    => UpdateStore m da -- ^ 'Store' for mirroring.
    -> a -- ^ Initial value.
    -> m (DBVar m da)
initDBVar store v = do
    writeS store v
    newWithCache (updateS store . Just) v

-- | Create a 'DBVar' that mirrors to a given 'Store',
-- and also loads its initial value from there.
-- Throws an exception if the value cannot be loaded.
loadDBVar
    ::  ( MonadSTM m, MonadThrow m, MonadEvaluate m, MonadMask m
        , Delta da
        )
    => UpdateStore m da -- ^ 'Store' for mirroring and for reading the initial value.
    -> m (DBVar m da)
loadDBVar store =
    loadS store >>= \case
        Left  e -> throwIO e
        Right a -> newWithCache (updateS store . Just) a

-- | Create 'DBVar' from an initial value and an update function
-- using a 'TVar' as in-memory cache.
--
-- Space: The value in the 'TVar' will be evaluated to weak head normal form.
--
-- Concurrency: The update function needs to be atomic even in the presence
-- of asynchronous exceptions.
newWithCache
    ::  ( MonadSTM m, MonadThrow m, MonadMask m, MonadEvaluate m
        , Delta da, a ~ Base da
        )
    => (a -> da -> m ()) -> a -> m (DBVar m da)
newWithCache update a = do
    cache  <- newTVarIO a
    locked <- newTVarIO False   -- lock for updating the cache
    pure $ DBVar
        { readDBVar_     = readTVarIO cache
        , modifyDBMaybe_ = \f -> do
            let before = atomically $ do
                    readTVar locked >>= \case
                        True  -> retry
                        False -> do
                            writeTVar locked True
                            readTVar cache
                after _ = atomically $ writeTVar locked False
                action old = do
                    let (mdelta, b) = f old
                    case mdelta of
                        Nothing    -> pure ()
                        Just delta -> do
                            new <- evaluate $ apply delta old
                            mask $ \restore -> do
                                -- We mask asynchronous exceptions here
                                -- to ensure that the TVar will be updated
                                -- whenever @update@ succeeds without exception.
                                restore $ update old delta
                                atomically $ writeTVar cache new
                    pure b
            bracket before after action
        }
