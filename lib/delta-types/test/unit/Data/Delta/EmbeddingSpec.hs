{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Data.Delta.EmbeddingSpec
    ( spec
    ) where

import Prelude

import Control.Exception
    ( Exception (..)
    , SomeException
    )
import Data.Delta.Core
    ( Delta (..)
    , Replace (..)
    , apply
    )
import Data.Delta.Embedding
    ( Embedding' (..)
    , Embedding
    , fromEmbedding
    , liftUpdates
    , mkEmbedding
    , pair
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , frequency
    , forAll
    , property
    , (===)
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "testEmbedding'" $ do
        it "prop_load_write" $
            prop_load_write testEmbedding'

        it "prop_update_apply" $
            prop_update_apply testEmbedding'

    describe "fromEmbedding . mkEmbedding'" $ do
        let testEmbedding'2 = fromEmbedding $ mkEmbedding testEmbedding'

        it "prop_load_write" $
            prop_load_write testEmbedding'2

        it "prop_update_apply" $
            prop_update_apply testEmbedding'2

    describe "pair" $ do
        let testEmbedding'3 =
                fromEmbedding
                    $ pair testEmbedding testEmbedding

        it "prop_load_write" $
            prop_load_write testEmbedding'3

        it "prop_update_apply" $
            prop_update_apply testEmbedding'3

    describe "liftUpdates" $ do
        it "load . apply" $ property $
            \(das :: [DeltaA]) (a :: A) ->
                let Embedding'{load} = testEmbedding'
                    (b, _) = liftUpdates testEmbedding das a
                in  toMaybe (load b)
                        ===  Just (apply das a)

        it "apply . load" $ property $
            \(das :: [DeltaA]) (a :: A) ->
                let Embedding'{write} = testEmbedding'
                    (b, dbs) = liftUpdates testEmbedding das a
                in  b  ===  apply dbs (write a)

-- | Law relating 'load' and 'write'.
prop_load_write
    :: ( Arbitrary (Base da), Show (Base da), Eq (Base da) )
    => Embedding' da db -> Property
prop_load_write Embedding'{load,write} =
    forAll arbitrary $ \a ->
        toMaybe (load (write a)) === Just a

-- | Law relating 'update' and 'load'.
prop_update_apply
    :: ( Delta da, Delta db, Arbitrary da, Arbitrary (Base da)
       , Show (Base da), Show da, Eq (Base da)
       )
    => Embedding' da db -> Property
prop_update_apply Embedding'{load,write,update} =
    forAll arbitrary $ \a ->
    forAll arbitrary $ \da ->
        let b = write a
            db = update a b da
        in  Just (apply da a)
                === toMaybe (load (apply db b))

toMaybe :: Either SomeException b -> Maybe b
toMaybe = either (const Nothing) Just

{-----------------------------------------------------------------------------
    Embeddings
------------------------------------------------------------------------------}
data NoPreimage = NoPreimage
    deriving Show

instance Exception NoPreimage

type A = Int
type DeltaA = Replace Int

type B = Maybe (A, Char)
data DeltaB
    = ChangeNone
    | ChangeA DeltaA
    | ChangeChar Char
    deriving Show

instance Delta DeltaB where
    type Base DeltaB = B
    apply ChangeNone b = b
    apply (ChangeA _ ) Nothing = Nothing
    apply (ChangeA da) (Just (a, c)) = Just (apply da a, c)
    apply (ChangeChar _) Nothing = Nothing
    apply (ChangeChar c) (Just (a, _)) = Just (a, c)

testEmbedding :: Embedding DeltaA DeltaB
testEmbedding = mkEmbedding testEmbedding'

testEmbedding' :: Embedding' DeltaA DeltaB
testEmbedding' = Embedding'
    { load = \b -> case b of
        Nothing -> Left $ toException NoPreimage
        Just (a,_) -> Right a
    , write = \a -> Just (a, 'X')
    , update = \_ _ da -> ChangeA da
    }

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance Arbitrary DeltaA where
    arbitrary = Replace <$> arbitrary

instance Arbitrary DeltaB where
    arbitrary = frequency
        [ (1, pure ChangeNone)
        , (4, ChangeA <$> arbitrary)
        , (4, ChangeChar <$> arbitrary)
        ]
