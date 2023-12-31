module StepperTests where

import Control.Applicative
import Control.Monad (guard)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import LoxArbitrary
import LoxStepper
import LoxSyntax
  ( Block (Block),
    Bop (Divide, Eq, Gt, Minus, Modulo, Plus, Times),
    Expression (ArrayIndex, FunctionCall, Op1, Op2, Val, Var),
    LValue (LArrayIndex, LName),
    Name,
    Statement (Assign, FunctionCallStatement, FunctionDef, Return, VarDecl),
    Uop (Neg, Not),
    Value (ArrayVal, BoolVal, ErrorVal, FunctionVal, FunctionValIncomplete, IntVal, NilVal, StringVal),
    loxAbs,
    loxAdvFunc,
    loxAnonFunc,
    loxArray,
    loxClosure,
    loxExp,
    loxFstClassFunc,
    loxMoreClosure,
    loxScope, loxAssignError, loxDeclareError, loxFunError,
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
                [("x", IntVal 3)]
            ),
            ( "_t1",
              Map.fromList
                [ ("y", BoolVal True)
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
                [("z", IntVal 3)]
            ),
            ( "_t2",
              Map.fromList
                [ ("4", BoolVal True)
                ]
            )
          ],
      parent = Just 0
    }

env3 :: Environment
env3 =
  Env
    { memory =
        Map.fromList
          [ ( globalTableName,
              Map.fromList
                [("y", IntVal 3), ("x", ArrayVal [IntVal 1, IntVal 2, IntVal 3])]
            ),
            ( "_t1",
              Map.fromList
                [ ("y", BoolVal True)
                ]
            )
          ],
      parent = Nothing
    }

envs1 :: Environments
envs1 = Map.fromList [(0, env1)]

envs2 = Map.fromList [(0, env1), (1, env2)]

envs3 :: Environments
envs3 = Map.fromList [(0, env3)]

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
               in let block = Block [statement]
                   in FunctionVal args block 0

functionEnv :: Environment
functionEnv =
  Env
    { memory =
        Map.fromList
          [ ( globalTableName,
              Map.fromList
                [("x", IntVal 3), ("f", functionVal)]
            ),
            ( "_t1",
              Map.fromList
                [ ("y", BoolVal True)
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
xref = ("_G", LName "x")

yref :: Reference
yref = ("_t1", LName "y")

zref :: Reference
zref = ("_t2", LName "4")

wref :: Reference
wref = ("_t1", LName "0")

arrayRef :: Reference
arrayRef = ("_G", LArrayIndex (LName "x") $ Val (IntVal 2))

arrayStore :: Store
arrayStore =
  St
    { environment = 0,
      environments = envs3,
      stack = stack1
    }

test_index :: Test
test_index =
  "index tests"
    ~: TestList
      [ S.evalState (index xref) initialStore ~?= ErrorVal "Cannot find value",
        S.evalState (index xref) extendedStore ~?= IntVal 3,
        S.evalState (index yref) initialStore ~?= ErrorVal "Cannot find value",
        S.evalState (index yref) extendedStore ~?= BoolVal True,
        S.evalState (index yref) extendedStore2 ~?= BoolVal True,
        S.evalState (index arrayRef) arrayStore ~?= IntVal 3
      ]

tref :: (String, Value)
tref = ("_G", StringVal "x")

test_update :: Test
test_update =
  "index tests"
    ~: TestList
      [ S.evalState (update xref (IntVal 4) >> update xref NilVal >> index xref) initialStore ~?= ErrorVal "Cannot find value",
        S.evalState (update yref (IntVal 5) >> index yref) extendedStore ~?= IntVal 5,
        S.evalState (update yref (IntVal 6) >> index yref) extendedStore ~?= IntVal 6,
        S.evalState (update yref (StringVal "hi") >> index yref) extendedStore2 ~?= StringVal "hi",
        S.evalState (update zref (IntVal 30) >> index zref) extendedStore2 ~?= IntVal 30,
        S.evalState (update wref (IntVal 30) >> index wref) extendedStore2 ~?= ErrorVal "Cannot find value"
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
        evaluate (Op2 (Val (IntVal 10)) Plus (Val (StringVal "hello"))) initialStore ~?= ErrorVal "Binary operation on invalid types",
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
      [ evaluate (Var "x") initialStore ~?= ErrorVal "Cannot find value",
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


expectedStoreFunc :: Store
expectedStoreFunc =
  St {environment = 0, environments = Map.fromList [(0, Env {memory = Map.fromList [("_G", Map.fromList [("t", FunctionVal ["z"] (Block [Assign (LName "x") (Op2 (Var "x") Plus (Val (IntVal 1))), Return (Op2 (Var "z") Plus (Val (IntVal 6)))]) 0), ("x", IntVal 2), ("y", IntVal 2), ("z", IntVal 9)])], parent = Nothing}), (1, Env {memory = Map.fromList [("_G", Map.fromList [("z", IntVal 2)])], parent = Just 0})], stack = Stk {curr = 0, par = Nothing}}

expectedLoxExp :: Store
expectedLoxExp = St {environment = 0, environments = Map.fromList [(0, Env {memory = Map.fromList [("_G", Map.fromList [("x1", IntVal 4), ("x2", ErrorVal "Binary operation on invalid types"), ("x3", NilVal), ("x4", ErrorVal "Binary operation on invalid types"), ("x5", BoolVal True), ("x6", BoolVal False), ("x7", BoolVal False)])], parent = Nothing})], stack = Stk {curr = 0, par = Nothing}}

expectedStoreAbs :: Store
expectedStoreAbs = St {environment = 0, environments = Map.fromList [(0, Env {memory = Map.fromList [("_G", Map.fromList [("x", IntVal 3)])], parent = Nothing}), (1, Env {memory = Map.fromList [("_G", Map.empty)], parent = Just 0})], stack = Stk {curr = 0, par = Nothing}}

expectedStoreScope :: Store
expectedStoreScope = St {environment = 0, environments = Map.fromList [(0, Env {memory = Map.fromList [("_G", Map.fromList [("x", IntVal 0), ("y", IntVal 10), ("z", IntVal 40)])], parent = Nothing}), (1, Env {memory = Map.fromList [("_G", Map.fromList [("x", IntVal 10)])], parent = Just 0})], stack = Stk {curr = 0, par = Nothing}}

expectedStoreAnon :: Store
expectedStoreAnon = St {environment = 0, environments = Map.fromList [(0, Env {memory = Map.fromList [("_G", Map.fromList [("test", FunctionVal ["a", "b"] (Block [Return (FunctionCall (Var "a") [Var "b"])]) 0), ("x", IntVal 2)])], parent = Nothing}), (1, Env {memory = Map.fromList [("_G", Map.fromList [("a", FunctionVal ["y"] (Block [Return (Op2 (Var "y") Plus (Val (IntVal 1)))]) 0), ("b", IntVal 1)])], parent = Just 0}), (2, Env {memory = Map.fromList [("_G", Map.fromList [("y", IntVal 1)])], parent = Just 0})], stack = Stk {curr = 0, par = Nothing}}

expectedStoreClosure :: Store
expectedStoreClosure = St {environment = 0, environments = Map.fromList [(0, Env {memory = Map.fromList [("_G", Map.fromList [("f2", FunctionVal [] (Block [Return (Var "outside")]) 0), ("outer", FunctionVal [] (Block [VarDecl "outside" (Val (IntVal 5)), FunctionDef "inner" [] (Block [Assign (LName "outside") (Op2 (Var "outside") Plus (Val (IntVal 1))), Return (Var "outside")]), Return (Var "inner")]) 0), ("outside", IntVal 7), ("t", FunctionVal [] (Block [Assign (LName "outside") (Op2 (Var "outside") Plus (Val (IntVal 1))), Return (Var "outside")]) 1), ("x", IntVal 6)])], parent = Nothing}), (1, Env {memory = Map.fromList [("_G", Map.fromList [("inner", FunctionVal [] (Block [Assign (LName "outside") (Op2 (Var "outside") Plus (Val (IntVal 1))), Return (Var "outside")]) 1), ("outside", IntVal 6)])], parent = Just 0}), (2, Env {memory = Map.fromList [("_G", Map.fromList [])], parent = Just 1})], stack = Stk {curr = 0, par = Nothing}}

expectedStoreArray :: Store
expectedStoreArray = St {environment = 0, environments = Map.fromList [(0, Env {memory = Map.fromList [("_G", Map.fromList [("x", ArrayVal [IntVal 1, IntVal 0, IntVal 3]), ("y", IntVal 3), ("z", ArrayVal [ArrayVal [IntVal 1, IntVal 4], ArrayVal [IntVal 2, IntVal 3]])])], parent = Nothing})], stack = Stk {curr = 0, par = Nothing}}

expectedStoreFirst :: Store
expectedStoreFirst = St {environment = 0, environments = Map.fromList [(0, Env {memory = Map.fromList [("_G", Map.fromList [("addSub", FunctionVal ["x"] (Block [FunctionDef "add" ["x"] (Block [Return (Op2 (Var "x") Plus (Val (IntVal 1)))]), FunctionDef "sub" ["x"] (Block [Return (Op2 (Var "x") Minus (Val (IntVal 1)))]), Return (FunctionCall (Var "sub") [FunctionCall (Var "add") [Var "x"]])]) 0), ("x", IntVal 2)])], parent = Nothing}), (1, Env {memory = Map.fromList [("_G", Map.fromList [("add", FunctionVal ["x"] (Block [Return (Op2 (Var "x") Plus (Val (IntVal 1)))]) 1), ("sub", FunctionVal ["x"] (Block [Return (Op2 (Var "x") Minus (Val (IntVal 1)))]) 1), ("x", IntVal 1)])], parent = Just 0}), (2, Env {memory = Map.fromList [("_G", Map.fromList [("x", IntVal 2)])], parent = Just 1}), (3, Env {memory = Map.fromList [("_G", Map.fromList [("add", FunctionVal ["x"] (Block [Return (Op2 (Var "x") Plus (Val (IntVal 1)))]) 3), ("sub", FunctionVal ["x"] (Block [Return (Op2 (Var "x") Minus (Val (IntVal 1)))]) 3), ("x", IntVal 1)])], parent = Just 0}), (4, Env {memory = Map.fromList [("_G", Map.fromList [("x", IntVal 2)])], parent = Just 3})], stack = Stk {curr = 0, par = Nothing}}

expectedStoreMoreClosure :: Store
expectedStoreMoreClosure = St {environment = 0, environments = Map.fromList [(0, Env {memory = Map.fromList [("_G", Map.fromList [("outer", FunctionVal [] (Block [VarDecl "outside" (Val (IntVal 0)), FunctionDef "inner" [] (Block [Assign (LName "outside") (Op2 (Var "outside") Plus (Val (IntVal 1))), Return (Var "outside")]), Return (Var "inner")]) 0), ("outside", IntVal 10), ("x", ArrayVal [FunctionVal [] (Block [Assign (LName "outside") (Op2 (Var "outside") Plus (Val (IntVal 1))), Return (Var "outside")]) 1, FunctionVal [] (Block [Assign (LName "outside") (Op2 (Var "outside") Plus (Val (IntVal 1))), Return (Var "outside")]) 2]), ("y", IntVal 3), ("z", IntVal 1)])], parent = Nothing}), (1, Env {memory = Map.fromList [("_G", Map.fromList [("inner", FunctionVal [] (Block [Assign (LName "outside") (Op2 (Var "outside") Plus (Val (IntVal 1))), Return (Var "outside")]) 1), ("outside", IntVal 3)])], parent = Just 0}), (2, Env {memory = Map.fromList [("_G", Map.fromList [("inner", FunctionVal [] (Block [Assign (LName "outside") (Op2 (Var "outside") Plus (Val (IntVal 1))), Return (Var "outside")]) 2), ("outside", IntVal 1)])], parent = Just 0}), (3, Env {memory = Map.fromList [("_G", Map.fromList [])], parent = Just 1}), (4, Env {memory = Map.fromList [("_G", Map.fromList [])], parent = Just 1}), (5, Env {memory = Map.fromList [("_G", Map.fromList [])], parent = Just 1}), (6, Env {memory = Map.fromList [("_G", Map.fromList [])], parent = Just 2})], stack = Stk {curr = 0, par = Nothing}}

tExecStepFunc :: Test
tExecStepFunc =
  "execStep function"
    ~: execStep loxAdvFunc initialStore
    ~?= Right expectedStoreFunc

tExecStepAbs :: Test
tExecStepAbs =
  "execStep exp"
    ~: execStep loxAbs initialStore
    ~?= Right expectedStoreAbs

tExecScope :: Test
tExecScope =
  "execStep exp"
    ~: execStep loxScope initialStore
    ~?= Right expectedStoreScope

tExecAnonFunc :: Test
tExecAnonFunc =
  "execStep exp"
    ~: execStep loxAnonFunc initialStore
    ~?= Right expectedStoreAnon

tExecClosure :: Test
tExecClosure =
  "execStep exp"
    ~: execStep loxClosure initialStore
    ~?= Right expectedStoreClosure

tExecArray :: Test
tExecArray =
  "execStep exp"
    ~: execStep loxArray initialStore
    ~?= Right expectedStoreArray

tExecFirst :: Test
tExecFirst =
  "execStep exp"
    ~: execStep loxFstClassFunc initialStore
    ~?= Right expectedStoreFirst

tExecMoreClosure :: Test
tExecMoreClosure =
  "execStep exp"
    ~: execStep loxMoreClosure initialStore
    ~?= Right expectedStoreMoreClosure


tExecAssignError :: Test
tExecAssignError =
  "execStep exp"
    ~: execStep loxAssignError initialStore
    ~?= Left "Variable y does not exists"

tExecDeclareError :: Test
tExecDeclareError =
  "execStep exp"
    ~: execStep loxDeclareError initialStore
    ~?= Left "Multiple definitons. Variable x already exists"

tExecFunError :: Test
tExecFunError =
  "execStep exp"
    ~: execStep loxFunError initialStore
    ~?= Left "Unknown Function"



test_execStep :: Test
test_execStep = TestList [tExecStepFunc, tExecStepAbs, tExecScope, tExecAnonFunc, tExecClosure, tExecArray, tExecFirst, tExecMoreClosure, tExecDeclareError, tExecAssignError, tExecFunError]

prop_evaluateNot :: Value -> Store -> Bool
prop_evaluateNot v s = evaluate (Op1 Not (Val v)) s == BoolVal (not $ toBool v)

prop_step_total :: Block -> Store -> Bool
prop_step_total b s = case S.runState (step b) s of
  (b', s') -> True

prop_stepExec :: Block -> QC.Property
prop_stepExec b =
  not (final b) QC.==> case result of
    Left _ -> QC.property True
    Right b1 -> final b1 QC.==> m1 == m2
  where
    (result, m1) = S.runState (boundedStep 100 b) initialStore
    m2 = exec b initialStore

prop_evalE_total :: Expression -> Store -> Bool
prop_evalE_total e s = case evaluate e s of
  NilVal -> True
  IntVal i -> i `seq` True
  BoolVal b -> b `seq` True
  StringVal s -> s `seq` True
  ArrayVal vs -> vs `seq` True
  FunctionValIncomplete ns bl -> ns `seq` bl `seq` True
  FunctionVal ns bl id -> ns `seq` bl `seq` id `seq` True
  ErrorVal s -> s `seq` True

test_all :: IO Counts
test_all = runTestTT $ TestList [test_execStep, evaluateTests, storeTests]

test_programs = runTestTT test_execStep

qc :: IO ()
qc = do
  putStrLn "evaluateNot"
  quickCheckN 100 prop_evaluateNot
  putStrLn "evalE_total"
  quickCheckN 100 prop_evalE_total
  putStrLn "step_total"
  quickCheckN 100 prop_step_total
  putStrLn "stepExec"
  quickCheckN 100 prop_stepExec
