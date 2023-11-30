module LoxStepper where

import Control.Applicative
import Data.Char qualified as Char
import LoxSyntax
import LoxParser
import ParserLib (Parser)
import ParserLib qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import State (State)
import State qualified as S
import Data.List qualified as List
import Control.Monad (guard)
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)


type Environment = Map Name Table

type Table = Map Value Value

data Store = St {environment :: Environment, parent :: Maybe Store} deriving (Eq, Show)

globalTableName  :: Name
globalTableName = "_G"

initialStore :: Store
initialStore = St { environment = Map.singleton globalTableName Map.empty, parent = Nothing}

extendedStore :: Store
extendedStore =
    St
        {
        environment = Map.fromList
            [ ( globalTableName,
                Map.fromList
                [ (StringVal "x", IntVal 3)]
            ),
            ( "_t1",
                Map.fromList
                [ (StringVal "y", BoolVal True)
                ]
            )
            ],
        parent = Nothing
        }

extendedStore2 :: Store
extendedStore2 =
  St
    { environment =
        Map.fromList
          [ ( globalTableName,
              Map.fromList
                [(StringVal "z", IntVal 3)]
            ),
            ( "_t2",
              Map.fromList
                [ (IntVal 4, BoolVal True)
                ]
            )
          ],
      parent = Just extendedStore
    }

type Reference = (Name, Value)

xref :: Reference
xref = ("_G", StringVal "x")

yref :: Reference
yref = ("_t1", StringVal "y")

zref :: Reference
zref = ("_t2", IntVal 4)

wref :: Reference
wref = ("_t1", IntVal 0)

index :: Reference -> State Store Value
index (t, n) = do
  store <- S.get
  let valMaybe = let env = environment store in env !? t >>= (!? n)
  let u :: Value = case parent store of 
        Nothing -> NilVal
        Just st -> let b = index (t,n) in S.evalState b st
  return $ fromMaybe u valMaybe


-- updateNoInsert :: Reference -> Value -> State Store ()
-- updateNoInsert (_, NilVal) _ = return ()
-- updateNoInsert (table, name) val = do
--   store <- S.get
--   let newStore :: Maybe Store =
--         do
--           oldEnv <- environment store !? table
--           let newTable =
--                 if val == NilVal
--                   then Map.delete name oldEnv
--                   else Map.insert name val oldEnv
--           return $ store {environment = Map.insert table newTable (environment store)}
--   S.put $ fromMaybe store newStore

-- update :: Reference -> Value -> State Store ()
-- update (_, NilVal) _ = return ()
-- update (table, name) val = do
--   store <- S.get
--   let newStore :: Maybe Store =
--         do
--           oldEnv <- environment store !? table
--           let newTable = Map.insert name val oldEnv
--           return $ store {environment = Map.insert table newTable (environment store)}
--   S.put $ fromMaybe store newStore


allocate :: Reference -> Value -> State Store ()
allocate (_, NilVal) _ = return ()
allocate (table, name) val = do
  store <- S.get
  let newStore :: Maybe Store =
        do
          oldTable <- environment store !? table
          guard ( isNothing $ Map.lookup name oldTable)
          let newTable = Map.insert name val oldTable
          return $ store { environment = Map.insert table newTable (environment store)}
  S.put $ fromMaybe store newStore



update :: Reference -> Value -> State Store ()
update (_, NilVal) _ = return ()
update ref@(table, name) val = do
  store <- S.get
  let updateInEnv :: Environment -> Maybe Environment
      updateInEnv env = do
        oldEnv <- env !? table
        let newTable = Map.update (\_ -> Just val)  name oldEnv
        return $ Map.insert table newTable env

  let updateRecursively :: Maybe Store -> Maybe Store
      updateRecursively Nothing = Nothing
      updateRecursively (Just st) =
        case updateInEnv (environment st) of
          Just newEnv -> Just $ st {environment = newEnv}
          Nothing -> do
            updatedParent <- updateRecursively (parent st)
            return $ st {parent = Just updatedParent}

  S.put $ fromMaybe store (updateRecursively (Just store))

test_index :: Test
test_index =
  "index tests" ~:
    TestList
      [ -- The global variable "x" is unitialized (i.e. not present in the initial store)
        S.evalState (index xref) initialStore ~?= NilVal,
        -- But there is a value for "x" available in the extended store
        S.evalState (index xref) extendedStore ~?= IntVal 3,
        -- If a table name is not found in the store, accessing its reference also returns nil.
        S.evalState (index yref) initialStore ~?= NilVal,
        -- We should also be able to access "t[1]" in the extended store
        S.evalState (index yref) extendedStore ~?= BoolVal True,
        -- Updates using the `nil` key are ignored
        S.execState (update ("_t1", NilVal) (IntVal 3)) extendedStore ~?= extendedStore,

        S.evalState (index yref) extendedStore2 ~?= BoolVal True
      ]

test_update :: Test
test_update =
  "index tests"
    ~: TestList
      [ -- If we assign to x, then we can find its new value
        -- If we assign to x, then remove it, we cannot find it anymore
        S.evalState (update xref (IntVal 4) >> update xref NilVal >> index xref) initialStore ~?= NilVal,
        -- If we assign to t.y, then we can find its new value
        S.evalState (update yref (IntVal 5) >> index yref) extendedStore ~?= IntVal 5,
        -- If we assign nil to t.y, then we cannot find it
        S.evalState (update yref NilVal >> index yref) extendedStore ~?= NilVal,

        S.evalState (update yref NilVal >> index yref) extendedStore2 ~?= NilVal,

        S.evalState (update zref (IntVal 30) >> index zref) extendedStore2 ~?= IntVal 30,

        S.evalState (update wref (IntVal 30) >> index wref) extendedStore2 ~?= NilVal
      ]


test_allocate :: Test
test_allocate =
  "allocate tests"
    ~: TestList
      [
        S.evalState (allocate xref (IntVal 4) >> index xref) initialStore ~?= IntVal 4,
        S.evalState (allocate xref (IntVal 4) >> index xref) extendedStore ~?= IntVal 3
      ]


-- >>> runTestTT test_index
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}


-- >>> runTestTT test_update
-- Counts {cases = 7, tried = 7, errors = 0, failures = 1}
