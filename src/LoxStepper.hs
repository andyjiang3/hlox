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

storeTests :: Test
storeTests = TestList [test_index, test_update, test_allocate]

-- >>>runTestTT test_update
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}


eval :: Expression -> State Store Value
eval (Var name) = index (globalTableName, StringVal name)
eval (Val v) = return v
eval (Op2 e1 o e2) = evalOp2 o <$> eval e1 <*> eval e2
eval (Op1 _o _e1) = eval _e1 >>= evalOp1 _o
eval _ = undefined


toBool :: Value -> Bool
toBool (BoolVal False) = False
toBool NilVal = False
toBool _ = True

evalOp1 :: Uop -> Value -> State Store Value
evalOp1 Neg (IntVal i) = return $ IntVal $ -i
evalOp1 Not v = return $ BoolVal $ not $ toBool v
evalOp1 _ _ = return NilVal

evalOp2 :: Bop -> Value -> Value -> Value
evalOp2 Plus (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
evalOp2 Minus (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
evalOp2 Times (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
evalOp2 Divide (IntVal _) (IntVal 0) = NilVal
evalOp2 Divide (IntVal i1) (IntVal i2) = IntVal (i1 `div` i2)
evalOp2 Modulo (IntVal _) (IntVal 0) = NilVal
evalOp2 Modulo (IntVal i1) (IntVal i2) = IntVal (i1 `mod` i2)
evalOp2 Eq v1 v2 = BoolVal $ v1 == v2
evalOp2 Gt v1 v2 = BoolVal $ v1 > v2
evalOp2 Ge v1 v2 = BoolVal $ v1 >= v2
evalOp2 Lt v1 v2 = BoolVal $ v1 < v2
evalOp2 Le v1 v2 = BoolVal $ v1 <= v2
evalOp2 _ _ _ = NilVal


evaluate :: Expression -> Store -> Value
evaluate e = S.evalState (eval e)


test_evaluate_unops :: Test
test_evaluate_unops =
  "evaluate unops"
    ~: TestList
      [ evaluate (Op1 Not (Val NilVal)) initialStore ~?= BoolVal True,
        evaluate (Op1 Not (Val (IntVal 3))) initialStore ~?= BoolVal False,
        evaluate (Op1 Neg (Val (IntVal 3))) initialStore ~?= IntVal (-3),
        evaluate (Op1 Neg (Val (IntVal 0))) initialStore ~?= IntVal 0
      ]

test_evaluate_binops :: Test
test_evaluate_binops =
  "evaluate binops"
    ~: TestList
      [ evaluate (Op2 (Val (IntVal 3)) Plus (Val (IntVal 5))) initialStore ~?= IntVal 8,
        evaluate (Op2 (Val (IntVal 10)) Minus (Val (IntVal 5))) initialStore ~?= IntVal 5,
        evaluate (Op2 (Val (IntVal 9)) Divide (Val (IntVal 2))) initialStore ~?= IntVal 4,
        evaluate (Op2 (Val (IntVal 4)) Times (Val (IntVal 4))) initialStore ~?= IntVal 16,
        evaluate (Op2 (Val (IntVal 9)) Divide (Val (IntVal 2))) initialStore ~?= IntVal 4,
        evaluate (Op2 (Val (IntVal 10)) Modulo (Val (IntVal 4))) initialStore ~?= IntVal 2,
        evaluate (Op2 (Val (IntVal 10)) Plus (Val (StringVal "hello"))) initialStore ~?= NilVal,
        evaluate (Op2 (Val (IntVal 4)) Eq (Val (IntVal 4))) initialStore ~?= BoolVal True,
        evaluate (Op2 (Val (IntVal 2)) Gt (Val (IntVal 6))) initialStore ~?= BoolVal False
      ]


test_evaluate_nested :: Test
test_evaluate_nested =
  "evaluate nested"
    ~: TestList
      [ evaluate (Op2 (Val (IntVal 7)) Plus (Op2 (Val (IntVal 3)) Plus (Val (IntVal 5)))) initialStore ~?= IntVal 15,
        evaluate (Op2 (Val (IntVal 5)) Eq (Op2 (Val (IntVal 8)) Minus (Val (IntVal 3)))) initialStore ~?= BoolVal True
      ]

test_evaluate_literals :: Test
test_evaluate_literals =
  "evaluate literals"
    ~: TestList
      [ evaluate (Val (IntVal 2)) initialStore ~?= IntVal 2,
        evaluate (Val (StringVal "hello")) initialStore ~?= StringVal "hello",
        evaluate (Val (BoolVal True)) initialStore ~?= BoolVal True
      ]

test_evaluate_vars :: Test
test_evaluate_vars =
  "evaluate vars"
    ~: TestList
      [ evaluate (Var "x") initialStore ~?= NilVal,
        evaluate (Var  "x") extendedStore ~?= IntVal 3,
        evaluate (Var  "x") extendedStore2 ~?= IntVal 3
      ]

evaluateTests = TestList [test_evaluate_nested, test_evaluate_literals, test_evaluate_vars, test_evaluate_binops, test_evaluate_unops]


-- >>> runTestTT evaluateTests
-- Counts {cases = 21, tried = 21, errors = 0, failures = 0}
