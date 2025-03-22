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
    ( UpdateStore
    , newStore
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
spec =
    describe "operations" $ do
        it "updateDBVar >> readDBVar" $ property $
            \(da :: DeltaA) a0 ->
                propertyIOSim $ do
                    store <- newUpdateStore
                    var <- initDBVar store a0
                    updateDBVar var da
                    a1 <- readDBVar var
                    pure $ a1 === apply da a0

{-----------------------------------------------------------------------------
    Test setup
------------------------------------------------------------------------------}

propertyIOSim :: Testable p => (forall s. IOSim s p) -> Property
propertyIOSim action = property $ runSimOrThrow action

newUpdateStore :: IOSim s (UpdateStore (IOSim s) DeltaA)
newUpdateStore = newStore

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
