{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Copyright   : © 2022-2023 IOHK, 2023-2025 Cardano Foundation
License     : Apache-2.0
Description : Computations that produce a delta and a result.

'Update' represents a computation which produces a delta and a result.

Similar to the 'Control.Monad.Trans.State.State' monad,
but involves a 'Delta' type.

Useful for composing updates to a 'DBVar', via 'onDBVar'.

Note: This module is preliminary.
-}
module Data.Delta.Update (
    -- * Update
    -- ** Type
      Update
    -- ** View
    , runUpdate
    , applyUpdate
    , onDBVar
    -- ** Combinators
    , nop
    , update
    , updateWithResult
    -- ** Helpers
    , updateWithError
    , updateWithResultAndError
    , updateMany
    , updateField
    ) where

import Prelude

import Data.DBVar
    ( DBVar
    , modifyDBMaybe
    )
import Data.Delta
    ( Delta (..)
    )

{-------------------------------------------------------------------------------
    Update
    Type, View
-------------------------------------------------------------------------------}
-- | A computation which inspects a value @a ~ Base da@
-- and produces a delta @da@ and a result of type @r@.
--
-- Related to the 'Control.Monad.Trans.State.State' monad:
-- The type @'Update' ('Data.Delta.Core.Replace' s) r@ is essentially equivalent to
-- @'Control.Monad.Trans.State.State' s r@.
newtype Update da r = Update { runUpdate_ :: Base da -> (Maybe da, r) }

-- | Run the 'Update' computation.
runUpdate :: (a ~ Base da) => Update da r -> a -> (Maybe da, r)
runUpdate = runUpdate_

-- | Semantics.
applyUpdate
    :: (Delta da, a ~ Base da)
    => Update da r -> a -> (a,r)
applyUpdate (Update g) a =
    case g a of
        (da, r) -> (da `apply` a, r)

-- | Apply an 'Update' to a 'DBVar'.
onDBVar
    :: (Monad m, Delta da)
    => DBVar m da -> Update da r -> m r
onDBVar dbvar = modifyDBMaybe dbvar . runUpdate

{-------------------------------------------------------------------------------
    Combinators
-------------------------------------------------------------------------------}
-- | Map results.
instance Functor (Update da) where
    fmap f (Update g) = Update $ \a ->
        case g a of
            (da, r) -> (da, f r)

-- | No operation.
--
-- Use the 'Functor' instance, specifically '(<$)'
-- to add results other than '()'.
nop :: Update da ()
nop = Update $ const (Nothing, ())

-- | Compute a delta.
update :: (a ~ Base da) => (a -> da) -> Update da ()
update f = Update $ \a -> (Just (f a), ())

-- | Compute a delta with result.
updateWithResult
    :: (a ~ Base da)
    => (a -> (da, r)) -- Delta with result.
    -> Update da r
updateWithResult f = Update $ \a ->
    case f a of
        (da, r) -> (Just da, r)

-- | Compute a delta or fail.
updateWithError
    :: (a ~ Base da)
    => (a -> Either e da)
    -> Update da (Either e ())
updateWithError f = Update $ \a ->
    case f a of
        Left e -> (Nothing, Left e)
        Right da -> (Just da, Right ())

-- | Compute a delta with result or fail.
updateWithResultAndError
    :: (a ~ Base da)
    => (a -> Either e (da, r))
    -> Update da (Either e r)
updateWithResultAndError f = Update $ \a ->
    case f a of
        Left e -> (Nothing, Left e)
        Right (da,r) -> (Just da, Right r)

-- | Lift an update for a single delta to a list of deltas.
updateMany
    :: Update da r
    -> Update [da] r
updateMany (Update g) = Update $ \a ->
    case g a of
        (Nothing, r) -> (Nothing, r)
        (Just da, r) -> (Just [da], r)

{- | Helper function for lifting the 'Update' from a
record field to the record.

Example:

@
data Pair a b = Pair a b
first :: Pair a b -> a

data DeltaPair da db
    = UpdateFirst da
    | UpdateSecond db

updateField first UpdateFirst
    :: (a -> Update da r)
    -> (Pair a b -> Update (DeltaPair da db) r)
@
-}
updateField
    :: (a ~ Base da, b ~ Base db)
    => (b -> a)
        -- ^ View field.
    -> (da -> db)
        -- ^ Lift delta to
    -> Update da r
    -> Update db r
updateField view embed (Update g) =
    Update $ lift . g . view
  where
    lift (mda, r) = (embed <$> mda, r)
