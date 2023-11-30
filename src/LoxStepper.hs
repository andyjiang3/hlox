module LoxStepper where

import Control.Applicative
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import LoxParser
import LoxSyntax
import ParserLib (Parser)
import ParserLib qualified as P
import State (State)
import State qualified as S
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

type Environment = Map Name Table

type Table = Map Value Value

data Store = St {environment :: Environment, parent :: Maybe Environment}

globalTableName :: Name
globalTableName = "_G"

initialStore :: Store
initialStore = St {environment = Map.singleton globalTableName Map.empty, parent = Nothing}

extendedStore :: Store
extendedStore =
  St
    { environment =
        Map.fromList
          [ ( globalTableName,
              Map.fromList
                [(StringVal "x", IntVal 3)]
            ),
            ( "_t1",
              Map.fromList
                [ (StringVal "y", BoolVal True)
                ]
            )
          ],
      parent = Nothing
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
  return $ fromMaybe NilVal valMaybe

index2 :: Reference -> State Store Value
index2 reference = do
  store <- S.get
  let valMaybe = lookupReference (environment store) reference
  case valMaybe of
    Just val -> return val
    Nothing -> case parent store of
      Just parentEnv -> do
        S.put store {environment = parentEnv}
        index2 reference
      Nothing -> return NilVal

lookupReference :: Environment -> Reference -> Maybe Value
lookupReference env (t, n) = do
  table <- env !? t
  table !? n

test_index :: Test
test_index =
  "index tests"
    ~: TestList
      [ -- The global variable "x" is unitialized (i.e. not present in the initial store)
        S.evalState (index xref) initialStore ~?= NilVal,
        -- But there is a value for "x" available in the extended store
        S.evalState (index xref) extendedStore ~?= IntVal 3,
        -- If a table name is not found in the store, accessing its reference also returns nil.
        S.evalState (index yref) initialStore ~?= NilVal,
        -- We should also be able to access "t[1]" in the extended store
        S.evalState (index yref) extendedStore ~?= BoolVal True
        -- Updates using the `nil` key are ignored
        -- S.execState (update ("_t1", NilVal) (IntVal 3)) extendedStore ~?= extendedStore
      ]

-- >>> runTestTT test_index
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}
