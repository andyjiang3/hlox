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

evalE :: Expression -> State Store Value
evalE (Var name) = index (globalTableName, StringVal name)
evalE (Val v) = return v
evalE (Op2 e1 o e2) = evalOp2 o <$> evalE e1 <*> evalE e2
evalE (Op1 o e1) = do
  store <- S.get
  v <- evalE e1
  return $ evalOp1 store o v
evalE _ = undefined

evalOp1 :: Store -> Uop -> Value -> Value
evalOp1 _ Neg (IntVal i) = IntVal (-i)
evalOp1 _ Not c = BoolVal (not (toBool c))
evalOp1 _ _ _ = NilVal

evalOp2 :: Bop -> Value -> Value -> Value
evalOp2 Plus (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
evalOp2 Minus (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
evalOp2 Times (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
evalOp2 Divide (IntVal i1) (IntVal i2) = if i2 /= 0 then IntVal (i1 `div` i2) else NilVal
evalOp2 Modulo (IntVal i1) (IntVal i2) = if i2 /= 0 then IntVal (i1 `mod` i2) else NilVal
evalOp2 Eq v1 v2 = BoolVal (v1 == v2)
evalOp2 Gt v1 v2 = BoolVal (v1 > v2)
evalOp2 Ge v1 v2 = BoolVal (v1 >= v2)
evalOp2 Lt v1 v2 = BoolVal (v1 < v2)
evalOp2 Le v1 v2 = BoolVal (v1 <= v2)
evalOp2 _ _ _ = NilVal

evaluate :: Expression -> Store -> Value
evaluate e = S.evalState (evalE e)

toBool :: Value -> Bool
toBool (BoolVal False) = False
toBool NilVal = False
toBool _ = True