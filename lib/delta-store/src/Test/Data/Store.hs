{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Copyright   : © 2022-2023 IOHK, 2023-2025 Cardano Foundation
License     : Apache-2.0
Description : Utilities for testing 'Store' implementations.

This module provides utilities for testing 'Store' implementations.

* 'prop_StoreUpdate' is a general property test that tests
  the laws for 'updateS'.
* 'genChain' generates random sequences of deltas.
* 'StoreUnitTest' provides a monadic DSL
  for writing example test cases for 'Store'.
-}
module Test.Data.Store
    ( -- * Store laws
      GenDelta
    , prop_StoreUpdate

    -- * Generators
    , Chain (..)
    , genChain
    , shrinkChain

    -- * Unit test DSL for developing a Store
    , StoreUnitTest
    , unitTestStore
    , applyS
    , checkLaw
    , reset
    , context
    , observe
    , ignore
    ) where

import Prelude

import Control.Exception
    ( throwIO
    )
import Control.Monad
    ( forM_
    )
import Control.Monad.Trans.Class
    ( lift
    )
import Control.Monad.Trans.RWS
    ( RWST
    , ask
    , censor
    , evalRWST
    , get
    , listen
    , put
    , tell
    )
import Data.Delta
    ( Delta (..)
    )
import Data.Either
    ( isRight
    )
import Data.Store
    ( Store (loadS, updateS, writeS)
    )
import Test.QuickCheck
    ( Gen
    , Property
    , conjoin
    , counterexample
    , forAll
    , forAllShrink
    , getSize
    , (===)
    )
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , monitor
    , run
    )

{-----------------------------------------------------------------------------
    Store laws
------------------------------------------------------------------------------}
-- | Given a value, generate a random delta that applies to this value.
type GenDelta da = Base da -> Gen da

-- | Chain of deltas and the results of their application.
--
-- The delta that is applied *last* appears in the list *first*.
data Chain da = Chain [(Base da, da)] (Base da)

instance Show da => Show (Chain da) where
    show (Chain adas _) = show . map snd $ adas

-- | Randomly generate a chain of deltas.
genChain :: Delta da => Gen (Base da) -> GenDelta da -> Gen (Chain da)
genChain gen0 more = do
    n <- getSize
    a0 <- gen0
    go n a0 [] a0
  where
    go 0 _  das a0 = pure $ Chain das a0
    go n alast das a0 = do
        da <- more alast
        let a = apply da alast
        go (n - 1) a ((a, da) : das) a0

-- | Shrink a chain of deltas.
shrinkChain :: Chain da -> [Chain da]
shrinkChain (Chain [] _) = []
shrinkChain (Chain das a0) =
    [ Chain [] a0, Chain [last das] a0, Chain (tail das) a0 ]

-- | Test whether the law on 'updateS' is satisfied.
--
-- Subsumes test for the law on 'writeS' / 'loadS'.
prop_StoreUpdate
    :: (Monad m, Delta da, Eq (Base da), Show da, Show (Base da))
    => (forall b. m b -> IO b)
    -- ^ Function to embed the monad in 'IO'
    -> m (Store m qa da)
    -- ^ Creation for 'Store' that is to be tested.
    -> Gen (Base da)
    -- ^ Generator for the initial value.
    -> GenDelta da
    -- ^ Generator for deltas.
    -> Property
prop_StoreUpdate toIO mkStore gen0 more =
    forAll gen0 $ \a0' ->
    forAllShrink (genChain (pure a0') more) shrinkChain $ \chain ->
        let Chain adas a0 = chain
            as = map fst adas ++ [a0]
            das = map snd adas
        in  counterexample ("\nUpdates applied:\n" <> unlines (map show as))
            $ monadicIO $ do
                ea <- run . toIO $ do
                    store <- mkStore
                    writeS store a0
                    -- first update is applied last!
                    let updates = reverse $ zip das (drop 1 as)
                    forM_ updates $ \(da, a) -> updateS store (Just a) da
                    loadS store
                case ea of
                    Left err -> run $ throwIO err
                    Right a -> do
                        monitor $ counterexample
                            $ "\nExpected:\n" <> show (head as)
                        monitor $ counterexample
                            $ "\nGot:\n" <> show a
                        assert $ a == head as

{-----------------------------------------------------------------------------
    DSL for developing
------------------------------------------------------------------------------}
-- | A monadic DSL to unit test a 'Store'.
newtype StoreUnitTest m qa da r = StoreUnitTest
    { runStoreUnitTest :: RWST
        (Store m qa da)
        [Property]
        (Base da, Base da, [da])
        m
        r
    } deriving (Functor, Applicative, Monad)

-- | Apply a delta to the current value.
applyS :: (Monad m, Delta da) => da -> StoreUnitTest m qa da ()
applyS r = StoreUnitTest $ do
    s <- ask
    (q, x, ds) <- get
    put (q, apply r x, r : ds)
    lift $ updateS s (Just x) r

-- | Check the store laws.
checkLaw
    :: (Monad m, Eq (Base da), Show (Base da), Show da)
    => StoreUnitTest m qa da ()
checkLaw = StoreUnitTest $ do
    (_, x, reverse -> ds) <- get
    x' <- ask >>= lift . loadS
    tell
        [ counterexample (show (ds, leftOf x')) (isRight x')
        , counterexample (show ds) $ rightOf x' === x
        ]
  where
    leftOf (Left x) = x
    leftOf _ = undefined
    rightOf (Right x) = x
    rightOf _ = undefined

-- | Reset the store state to the initial value.
reset :: Monad m => StoreUnitTest m qa da ()
reset = StoreUnitTest $ do
    s <- ask
    (q, _, _) <- get
    lift $ writeS s q
    put (q, q, [])

-- | Run a unit test for a 'Store'.
unitTestStore
    :: (Monad m, Eq (Base da), Show (Base da), Show da)
    => Base da
    -> Store m qa da
    -> StoreUnitTest m qa da a
    -> m Property
unitTestStore x s f =
    conjoin . snd
        <$> evalRWST (runStoreUnitTest (f >> checkLaw)) s (x, x, [])

-- | Add a context to test.
context
    :: Monad m
    => (Property -> Property)
    -> StoreUnitTest m qa da x
    -> StoreUnitTest m qa da x
context d f = StoreUnitTest $ do
    (x, w) <- listen $ runStoreUnitTest f
    tell $ fmap d w
    pure x

-- | Observe a property on the current value of the store.
observe :: Monad m => (Base da -> Property) -> StoreUnitTest m qa da ()
observe f = StoreUnitTest $ do
    (_, s, _) <- get
    tell [f s]

-- | Ignore the properties of a sub-test.
ignore :: Monad m => StoreUnitTest m qa da x -> StoreUnitTest m qa da x
ignore = StoreUnitTest . censor (const []) . runStoreUnitTest
