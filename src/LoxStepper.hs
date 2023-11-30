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
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)


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
            ( "_t1",
              Map.fromList
                [ (StringVal "w", BoolVal True)
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

index :: Reference -> State Store Value
index (t, n) = do
  store <- S.get
  let valMaybe = let env = environment store in env !? t >>= (!? n)
  let u :: Value = case parent store of 
        Nothing -> NilVal
        Just st -> let b = index (t,n) in S.evalState b st
  return $ fromMaybe u valMaybe


update :: Reference -> Value -> State Store ()
update (_, NilVal) _ = return ()
update (table, name) val = do
  store <- S.get
  let newStore :: Maybe Store =
        do
          oldEnv <- environment store !? table
          let newTable =
                if val == NilVal
                  then Map.delete name oldEnv
                  else Map.insert name val oldEnv
          return $ store {environment = Map.insert table newTable (environment store)}
  S.put $ fromMaybe store newStore

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

-- >>> runTestTT test_index
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

