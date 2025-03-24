{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Database.Table.SchemaSpec
    ( spec
    ) where

import Prelude

import Data.Text
    ( Text
    )
import Database.Table.Schema
    ( Col
    , Row
    , Table
    , (:.)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "example types" $ do
        it "compile" $
            exampleRow == exampleRow

type ExampleTable =
    Table "person"
        :. Col "name" Text
        :. Col "birthyear" Int

exampleRow :: Row ExampleTable
exampleRow = ("Ada Lovelace", 1815)
