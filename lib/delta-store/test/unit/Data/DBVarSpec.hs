{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Copyright   : © 2022-2023 IOHK, 2023-2025 Cardano Foundation
License     : Apache-2.0
-}
module Data.DBVarSpec
    ( spec
    ) where

import Prelude

import Control.Concurrent.Class.MonadSTM
    ( atomically
    , newTVarIO
    , readTVar
    , writeTVar
    )
import Control.Exception
    ( toException
    )
import Control.Monad.Class.MonadAsync
    ( concurrently_
    , race
    )
import Control.Monad.Class.MonadThrow
    ( throwIO
    )
import Control.Monad.Class.MonadTimer
    ( threadDelay
    )
import Control.Monad.IOSim
    ( IOSim
    , runSimOrThrow
    )
import Data.Delta
    ( Delta (..)
    )
import Data.DBVar
    ( initDBVar
    , updateDBVar
    , readDBVar
    )
import Data.Store
    ( NotInitialized (..)
    , UpdateStore
    , mkUpdateStore
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Property
    , Testable (property)
    , elements
    , oneof
    , vectorOf
    , (===)
    )

spec :: Spec
spec = do
    describe "operations" $ do
        it "updateDBVar >> readDBVar" $ property $
            \(da :: DeltaA) a0 ->
                propertyIOSim $ do
                    store <- newDelayedStore 0
                    var <- initDBVar store a0
                    updateDBVar var da
                    a1 <- readDBVar var
                    pure $ a1 === apply da a0

    describe "concurrency" $ do
        it "readDBVar does not block" $ property $
            \(da :: DeltaA) a0 ->
                propertyIOSim $ do
                    store <- newDelayedStore 1000
                    var <- initDBVar store a0
                    Right a1 <- race
                        (updateDBVar var da)
                        (threadDelay 500 >> readDBVar var)
                    pure $ a1 === a0

        it "updateDBVar blocks" $ property $
            \(da10 :: DeltaA) da21 a0 ->
                propertyIOSim $ do
                    store <- newDelayedStore 1000
                    var <- initDBVar store a0
                    concurrently_
                        (updateDBVar var da10)
                        (threadDelay 500 >> updateDBVar var da21)
                    a2 <- readDBVar var
                    pure $ a2 === apply [da21, da10] a0

{-----------------------------------------------------------------------------
    Test setup
------------------------------------------------------------------------------}

propertyIOSim :: Testable p => (forall s. IOSim s p) -> Property
propertyIOSim action = property $ runSimOrThrow action

-- | 'Store' in volatile memory that invalidates the value
-- while performing an expensive update operation.
newDelayedStore
    :: forall da s. Delta da => Int -> IOSim s (UpdateStore (IOSim s) da)
newDelayedStore delay = do
    let broken = Left $ toException NotInitialized
    ref <- newTVarIO broken
    let load = atomically (readTVar ref)
        write = atomically . writeTVar ref . Right
        update :: Maybe (Base da) -> da -> IOSim s ()
        update _ da = do
            ea <- atomically $ do
                ea <- readTVar ref
                writeTVar ref broken    -- invalidate the value 
                pure ea
            threadDelay delay           -- wait for a long time
            case ea of
                Left e -> throwIO e
                Right a -> atomically . writeTVar ref . Right $ apply da a
    pure $ mkUpdateStore load write update

{-----------------------------------------------------------------------------
    Delta type for testing
------------------------------------------------------------------------------}

newtype A = A { unA :: [Int] }
    deriving (Show, Eq)

data DeltaA
    = AddOne
    | AddTwo
    | Drop
    deriving (Show, Eq)

instance Delta DeltaA where
    type Base DeltaA = A
    apply AddOne = A . (1:) . unA
    apply AddTwo = A . (2:) . unA
    apply Drop = A . (drop 1) . unA

instance Arbitrary A where
    arbitrary = A <$> oneof (map (\n -> vectorOf n arbitrary) [0,1,2])

instance Arbitrary DeltaA where
    arbitrary = elements [AddOne, AddTwo, Drop]
