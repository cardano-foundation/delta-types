{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Database.Table.SQLite.SimpleSpec
    ( spec
    ) where

import Prelude

import Control.Exception
    ( SomeException
    , catch
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Database.Table.Schema
    ( Col (..)
    , Row
    , Table
    , (:.)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )

import qualified Database.Table.SQLite.Simple as Sql

spec :: Spec
spec = do
    describe "SqlM examples" $ do
        it "doc example" $
            testExample
        it "various operations" $
            testExample2
        it "exceptions" $
            testExceptions

-- TODO: Add tests regarding comparison operators such as >. and NULL.

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}

type TablePerson =
    Table "person"
        :. Col "name" Text
        :. Col "birthyear" Int

tablePerson :: Proxy TablePerson
tablePerson = Proxy

colName :: Col "name" Text
colName = Col

colBirthYear :: Col "birthyear" Int
colBirthYear = Col

exampleRow :: Row TablePerson
exampleRow = ("Ada Lovelace", 1815)

-- | Example from the documentation.
example :: Sql.SqlM Int
example = do
    Sql.createTable tablePerson
    Sql.insertOne ("Babbage", 1791) tablePerson
    Sql.insertOne exampleRow tablePerson
    length <$> Sql.selectWhere (colBirthYear Sql.>. 1800) tablePerson

testExample :: IO ()
testExample = do
    rowCount <- Sql.withConnection ":memory:"
        $ Sql.runSqlM example
    assert "" $ rowCount == 1

-- | Sequence of database operations.
example2 :: Sql.SqlM [Row TablePerson]
example2 = do
    Sql.createTable tablePerson
    Sql.insertOne ("Neko", 1603) tablePerson
    Sql.deleteWhere (colName Sql.==. "Neko") tablePerson
    Sql.insertOne ("Babbage", 1791) tablePerson
    Sql.insertOne ("William", 1805) tablePerson
    Sql.insertOne ("Bada", 1815) tablePerson
    Sql.updateWhere
        (colName Sql.==. "Bada")
        [colName Sql.=. "Ada"]
        tablePerson
    Sql.selectWhere
        (colName Sql./=. "William" Sql.&&. colBirthYear Sql.>. 1800)
        tablePerson

rowsFinal :: [Row TablePerson]
rowsFinal = [("Ada",1815)]

testExample2 :: IO ()
testExample2 = do
    rows <- Sql.withConnection ":memory:" $ Sql.runSqlM example2
    assert (show rows)
        $ rows == rowsFinal

-- | Test 
testExceptions :: IO ()
testExceptions = do
    Sql.withConnection ":memory:" $ \conn -> do
        _ <- Sql.runSqlM example2 conn
        rowsBefore <- Sql.runSqlM (Sql.selectAll tablePerson) conn
        Sql.runSqlM
            ( do
                Sql.deleteWhere (colBirthYear Sql.>. 1800) tablePerson
                error "oops"
            ) conn `catch` (\(_ :: SomeException) -> pure ())
        rowsAfter <- Sql.runSqlM (Sql.selectAll tablePerson) conn
        assert (show rowsBefore <> " " <> show rowsAfter)
            $ rowsBefore == rowsAfter

assert :: String -> Bool -> IO ()
assert _ True = pure ()
assert s False = error $ "Assertion failed: " <> s
