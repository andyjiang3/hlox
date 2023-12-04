module StepperTests where

import LoxStepper
import Control.Applicative
import Data.Char qualified as Char
import LoxSyntax
    ( Name,
      Expression(Var, Op1, FunctionCall, Op2, Val),
      Statement(Return, Assign),
      LValue(LName),
      Block(Block),
      Value(IntVal, FunctionVal, NilVal, BoolVal, StringVal),
      Bop(Plus, Times, Divide, Modulo, Gt, Eq, Minus),
      Uop(Neg, Not),
      loxAbs,
      loxAdvFunc,
      loxExp )
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import State (State)
import State qualified as S
import Data.List qualified as List
import Control.Monad (guard)
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)

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


functionVal :: Value
functionVal =
  let name = Var "f"
   in let args :: [Name] = ["arg1"]
       in let returnExp :: Expression = Op2 (Val (IntVal 1)) Plus (Var "arg1")
           in let statement = Return returnExp
               in let block = Block []
                   in FunctionVal args block

functionStore :: Store
functionStore =
  St
    { environment =
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



intSt :: Store
intSt =
  St {environment = Map.fromList [("_G", Map.fromList [(StringVal "u", IntVal 1), (StringVal "z", IntVal 3)])], parent = Just (St {environment = Map.fromList [("_G", Map.fromList [(StringVal "t", FunctionVal ["z"] (Block [Assign (LName "x") (Op2 (Var "x") Plus (Val (IntVal 4))), Assign (LName "z") (Op2 (Var "z") Plus (Val (IntVal 1))), Return (Var "x")])), (StringVal "x", IntVal 12), (StringVal "y", IntVal 2)])], parent = Nothing})}
op :: Statement
op = Assign (LName "x") (Op2 (Var "x") Plus (Val (IntVal 4)))

tref :: (String, Value)
tref = ("_G", StringVal "x")

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

        S.evalState (update wref (IntVal 30) >> index wref) extendedStore2 ~?= NilVal,

        S.evalState (evalS op >> index tref) intSt ~?= IntVal 16,

        S.evalState (update tref (IntVal 30) >> index tref) intSt ~?= IntVal 30


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
-- Counts {cases = 8, tried = 8, errors = 0, failures = 0}



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




functionExpression :: Expression
functionExpression = FunctionCall (Var "f") [Val (IntVal 1)]


test_evaluate_functions :: Test
test_evaluate_functions =
  "evaluate functions"
    ~: TestList
      [ evaluate functionExpression functionStore ~?= IntVal 2]


evaluateTests = TestList [test_evaluate_nested, test_evaluate_literals, test_evaluate_vars, test_evaluate_binops, test_evaluate_unops]


-- >>> runTestTT evaluateTests
-- Counts {cases = 21, tried = 21, errors = 0, failures = 0}



expectedStoreFunc :: Store
expectedStoreFunc = St
        {
        environment = Map.fromList
            [ ( globalTableName,
                Map.fromList
                [(StringVal "x", IntVal 2), (StringVal "z",IntVal 2), (StringVal "y", IntVal 2), (StringVal "t", FunctionVal ["z"] (Block [Assign (LName "x") (Op2 (Var "x") Plus (Val (IntVal 1))), Return (Var "z")]))]
            )
            ],
        parent = Nothing
        }


expectedLoxExp :: Store
expectedLoxExp =
  St {
        environment = Map.fromList [ ("_G", Map.fromList [ 
            (StringVal "x1", IntVal 4),
             (StringVal "x2", NilVal),
              (StringVal "x3", NilVal),
              (StringVal "x4", NilVal), 
              (StringVal "x5", BoolVal True),
               (StringVal "x6", BoolVal False), 
               (StringVal "x7", BoolVal False)
               ])],
        parent = Nothing
    }

expectedStoreAbs :: Store
expectedStoreAbs = St {environment = Map.fromList [("_G", Map.fromList [(StringVal "x", IntVal 3)])], parent = Nothing}

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

-- >>> runTestTT test_execStep
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}



