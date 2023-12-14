module StepperTests where

import Control.Applicative
import Control.Monad (guard)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import LoxStepper
import LoxArbitrary
import LoxSyntax
  ( Block (Block),
    Bop (Divide, Eq, Gt, Minus, Modulo, Plus, Times),
    Expression (FunctionCall, Op1, Op2, Val, Var),
    LValue (LName),
    Name,
    Statement (Assign, Return),
    Uop (Neg, Not),
    Value (BoolVal, FunctionVal, IntVal, NilVal, StringVal),
    loxAbs,
    loxAdvFunc,
    loxExp,
  )
import State (State)
import State qualified as S
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

env1 :: Environment
env1 =
  Env
    { memory =
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

env2 :: Environment
env2 =
  Env
    { memory =
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
      parent = Just 0
    }

envs1 :: Environments
envs1 = Map.fromList [(0, env1)]

envs2 = Map.fromList [(0, env1), (1, env2)]

stack1 = Stk {curr = 0, par = Nothing}

stack2 = Stk {curr = 0, par = Just stack1}

extendedStore :: Store
extendedStore =
  St
    { environment = 0,
      environments = envs1,
      stack = stack1
    }

extendedStore2 :: Store
extendedStore2 =
  St
    { environment = 1,
      environments = envs2,
      stack = stack2
    }

functionVal :: Value
functionVal =
  let name = Var "f"
   in let args :: [Name] = ["arg1"]
       in let returnExp :: Expression = Op2 (Val (IntVal 1)) Plus (Var "arg1")
           in let statement = Return returnExp
               in let block = Block []
                   in FunctionVal args block 0

functionEnv :: Environment
functionEnv =
  Env
    { memory =
        Map.fromList
          [ ( globalTableName,
              Map.fromList
                [(StringVal "x", IntVal 3), (StringVal "f", functionVal)]
            ),
            ( "_t1",
              Map.fromList
                [ (StringVal "y", BoolVal True)
                ]
            )
          ],
      parent = Nothing
    }

functionEnvs :: Environments
functionEnvs = Map.fromList [(0, functionEnv)]

functionStore :: Store
functionStore =
  St
    { environment = 0,
      environments = functionEnvs,
      stack = stack1
    }

xref :: Reference
xref = ("_G", StringVal "x")

yref :: Reference
yref = ("_t1", StringVal "y")

zref :: Reference
zref = ("_t2", IntVal 4)

wref :: Reference
wref = ("_t1", IntVal 0)

test_index :: Test
test_index =
  "index tests"
    ~: TestList
      [ S.evalState (index xref) initialStore ~?= NilVal,
        S.evalState (index xref) extendedStore ~?= IntVal 3,
        S.evalState (index yref) initialStore ~?= NilVal,
        S.evalState (index yref) extendedStore ~?= BoolVal True,
        S.execState (update ("_t1", NilVal) (IntVal 3)) extendedStore ~?= extendedStore,
        S.evalState (index yref) extendedStore2 ~?= BoolVal True
      ]

-- >>> runTestTT test_index
-- Counts {cases = 5, tried = 5, errors = 0, failures = 1}

tref :: (String, Value)
tref = ("_G", StringVal "x")

test_update :: Test
test_update =
  "index tests"
    ~: TestList
      [ S.evalState (update xref (IntVal 4) >> update xref NilVal >> index xref) initialStore ~?= NilVal,
        S.evalState (update yref (IntVal 5) >> index yref) extendedStore ~?= IntVal 5,
        S.evalState (update yref NilVal >> index yref) extendedStore ~?= NilVal,
        S.evalState (update yref NilVal >> index yref) extendedStore2 ~?= NilVal,
        S.evalState (update zref (IntVal 30) >> index zref) extendedStore2 ~?= IntVal 30,
        S.evalState (update wref (IntVal 30) >> index wref) extendedStore2 ~?= NilVal
      ]

test_allocate :: Test
test_allocate =
  "allocate tests"
    ~: TestList
      [ S.evalState (allocate xref (IntVal 4) >> index xref) initialStore ~?= IntVal 4,
        S.evalState (allocate xref (IntVal 4) >> index xref) extendedStore ~?= IntVal 3
      ]

storeTests :: Test
storeTests = TestList [test_index, test_update, test_allocate]

-- >>>runTestTT storeTests
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

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
        evaluate (Var "x") extendedStore ~?= IntVal 3,
        evaluate (Var "x") extendedStore2 ~?= IntVal 3
      ]

functionExpression :: Expression
functionExpression = FunctionCall (Var "f") [Val (IntVal 1)]

test_evaluate_functions :: Test
test_evaluate_functions =
  "evaluate functions"
    ~: TestList
      [evaluate functionExpression functionStore ~?= IntVal 2]

evaluateTests = TestList [test_evaluate_nested, test_evaluate_literals, test_evaluate_vars, test_evaluate_binops, test_evaluate_unops]

-- >>> runTestTT evaluateTests
-- Counts {cases = 21, tried = 21, errors = 0, failures = 0}

expectedStoreFunc :: Store
expectedStoreFunc = St {environment = 0, 
environments = Map.fromList [
  (0, Env {memory = Map.fromList
   [("_G", Map.fromList
    [(StringVal "t", FunctionVal ["z"] (Block [Assign (LName "x") (Op2 (Var "x") Plus (Val (IntVal 1))), Return (Var "z")]) 0), 
    (StringVal "x", IntVal 2), (StringVal "y", IntVal 2), (StringVal "z", IntVal 2)])], parent = Nothing}),
      (1, Env {memory = Map.fromList [("_G", Map.fromList [(StringVal "z", IntVal 2)])], parent = Just 0})], stack = Stk {curr = 0, par = Nothing}}

expectedLoxExp :: Store
expectedLoxExp = St {environment = 0, environments = Map.fromList [(0, Env {memory = Map.fromList [("_G", Map.fromList [(StringVal "x1", IntVal 4), (StringVal "x2", NilVal), (StringVal "x3", NilVal), (StringVal "x4", NilVal), (StringVal "x5", BoolVal True), (StringVal "x6", BoolVal False), (StringVal "x7", BoolVal False)])], parent = Nothing})], stack = Stk {curr = 0, par = Nothing}}

expectedStoreAbs :: Store
expectedStoreAbs = St {environment = 0, environments = Map.fromList [(0, Env {memory = Map.fromList [("_G", Map.fromList [(StringVal "x", IntVal 3)])], parent = Nothing}), (1, Env {memory = Map.fromList [("_G", Map.empty)], parent = Just 0})], stack = Stk {curr = 0, par = Nothing}}

tExecStepFunc :: Test
tExecStepFunc =
  "execStep function"
    ~: execStep loxAdvFunc initialStore
    ~?= expectedStoreFunc

tExecStepExp :: Test
tExecStepExp =
  "execStep exp"
    ~: execStep loxExp initialStore
    ~?= expectedLoxExp

tExecStepAbs :: Test
tExecStepAbs =
  "execStep exp"
    ~: execStep loxAbs initialStore
    ~?= expectedStoreAbs

test_execStep :: Test
test_execStep = TestList [tExecStepFunc, tExecStepAbs, tExecStepExp]

-- -- -- >>> runTestTT test_execStep
-- -- -- Counts {cases = 3, tried = 3, errors = 0, failures = 0}




--- Propert based testing

prop_evaluateNot :: Value -> Store -> Bool
prop_evaluateNot v s = evaluate (Op1 Not (Val v)) s == BoolVal (not $ toBool v)


prop_step_total :: Block -> Store -> Bool
prop_step_total b s = case S.runState (step b) s of
  (b', s') -> True

prop_stepExec :: Block -> QC.Property
prop_stepExec b =
  not (final b) QC.==> final b1 QC.==> m1 == m2
  where
    (b1, m1) = S.runState (boundedStep 100 b) initialStore
    m2 = exec b initialStore


prop_evalE_total :: Expression -> Store -> Bool
prop_evalE_total e s = case evaluate e s of
  NilVal -> True
  IntVal i -> i `seq` True
  BoolVal b -> b `seq` True
  StringVal s -> s `seq` True
  _ -> undefined


qc :: IO ()
qc = do
  putStrLn "evaluateNot"
  quickCheckN 100 prop_evaluateNot
  putStrLn "evalE_total"
  quickCheckN 100 prop_evalE_total
  putStrLn "step_total"
  -- quickCheckN 100 prop_step_total
  -- putStrLn "stepExec"
  -- quickCheckN 100 prop_stepExec