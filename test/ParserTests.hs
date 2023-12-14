module ParserTests where

import Control.Applicative
import Data.Char qualified as Char
import LoxArbitrary
import LoxParser
import LoxSyntax
import ParserLib (Parser)
import ParserLib qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()]
    ]

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]

test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
      P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
      P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
      P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
    ]

test_funcValP :: Test
test_funcValP =
  TestList
    [ P.parse funcValP "\\(x1, x2) { y=4 }" ~?= Right (FunctionValIncomplete ["x1", "x2"] (Block [Assign (LName "y") (Val (IntVal 4))])),
      P.parse (many funcValP) "\\(x1, x2) { y=4 } \\(x1, x2) { y=4 }" ~?= Right [FunctionValIncomplete ["x1", "x2"] (Block [Assign (LName "y") (Val (IntVal 4))]), FunctionValIncomplete ["x1", "x2"] (Block [Assign (LName "y") (Val (IntVal 4))])]
    ]

test_arrayValP :: Test
test_arrayValP =
  TestList
    [ P.parse arrayValP "[1, 2, 3]" ~?= Right (ArrayVal [IntVal 1, IntVal 2, IntVal 3]),
      P.parse arrayValP "[\"a\", \"b\", \"c\"]" ~?= Right (ArrayVal [StringVal "a", StringVal "b", StringVal "c"]),
      P.parse arrayValP "[]" ~?= Right (ArrayVal []),
      P.parse (many arrayValP) "[1, 2, 3] [1, 2, 3]" ~?= Right [ArrayVal [IntVal 1, IntVal 2, IntVal 3], ArrayVal [IntVal 1, IntVal 2, IntVal 3]]
    ]

tParseFiles :: Test
tParseFiles =
  "parse files"
    ~: TestList
      [ "abs" ~: p "test/programs/abs.lox" loxAbs,
        "exp" ~: p "test/programs/exp.lox" loxExp,
        "basic func" ~: p "test/programs/basic_func.lox" loxBasicFunc,
        "adv func" ~: p "test/programs/adv_func.lox" loxAdvFunc
      ]
  where
    p fn ast = do
      result <- parseLoxFile fn
      case result of
        (Left _) -> assert False
        (Right ast') -> assert (ast == ast')

test_comb =
  "parsing combinators"
    ~: TestList
      [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
        P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc",
        P.parse (stringP "a") "a" ~?= Right (),
        P.parse (stringP "a") "b" ~?= Left "No parses",
        P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
        P.parse (constP "&" 'a') "&  " ~?= Right 'a',
        P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
        P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]" ~?= Right [1, 1, 1]
      ]

test_value =
  "parsing values"
    ~: TestList
      [ P.parse (many intValP) "1 2\n 3" ~?= Right [IntVal 1, IntVal 2, IntVal 3],
        P.parse (many boolValP) "true false\n true" ~?= Right [BoolVal True, BoolVal False, BoolVal True],
        P.parse (many nilValP) "nil nil\n nil" ~?= Right [NilVal, NilVal, NilVal],
        P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
        P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
        P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
        P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
      ]

test_exp =
  "parsing expressions"
    ~: TestList
      [ P.parse (many varP) "x y z" ~?= Right ["x", "y", "z"],
        P.parse varP "(x.y[1]).z" ~?= Left "No parses",
        P.parse (many varP) "x sfds _ nil" ~?= Right ["x", "sfds", "_"],
        P.parse (many uopP) "- - !" ~?= Right [Neg, Neg, Not],
        P.parse (many uopP) "! - test hi" ~?= Right [Not, Neg],
        P.parse (many bopP) "+ >= test" ~?= Right [Plus, Ge],
        P.parse (many bopP) "+ - * / % or and == != > >= < <=" ~?= Right [Plus, Minus, Times, Divide, Modulo, Or, And, Eq, Ne, Gt, Ge, Lt, Le],
        P.parse (funcCallExpP expP) "f(a1)" ~?= Right (FunctionCall (Var "f") [Var "a1"]),
        P.parse (funcCallExpP expP) "f(a1, a2)" ~?= Right (FunctionCall (Var "f") [Var "a1", Var "a2"]),
        P.parse arrayConsP "[1, 2, 3]" ~?= Right (ArrayCons [Val (IntVal 1), Val (IntVal 2), Val (IntVal 3)]),
        P.parse (arrayIndexExpP expP) "x[1]" ~?= Right (ArrayIndex (Var "x") (Val (IntVal 1))),
        P.parse (many arrayConsP) "[1, 2, 3] [4, 5, 6]" ~?= Right [ArrayCons [Val (IntVal 1), Val (IntVal 2), Val (IntVal 3)], ArrayCons [Val (IntVal 4), Val (IntVal 5), Val (IntVal 6)]]
      ]

test_stat =
  "parsing statements"
    ~: TestList
      [ P.parse statementP ";" ~?= Right Empty,
        P.parse statementP "y=4" ~?= Right (Assign (LName "y") (Val (IntVal 4))),
        P.parse statementP "var x=3" ~?= Right (VarDecl "x" (Val (IntVal 3))),
        P.parse statementP "x[1] = 3" ~?= Right (Assign (LArrayIndex (LName "x") (Val (IntVal 1))) (Val (IntVal 3))),
        P.parse statementP "x[1][2] = 2" ~?= Right (Assign (LArrayIndex (LArrayIndex (LName "x") (Val (IntVal 1))) (Val (IntVal 2))) (Val (IntVal 2))),
        P.parse statementP "var x = [1, 2, 3]" ~?= Right (VarDecl "x" (ArrayCons [Val (IntVal 1), Val (IntVal 2), Val (IntVal 3)])),
        P.parse statementP "if (x) { y=4 } else { y=5 }" ~?= Right (If (Var "x") (Block [Assign (LName "y") (Val (IntVal 4))]) (Block [Assign (LName "y") (Val (IntVal 5))])),
        P.parse statementP "for (var x=3; x<6; x=x+1) { y=4 }" ~?= Right (For (VarDecl "x" (Val (IntVal 3))) (Op2 (Var "x") Lt (Val (IntVal 6))) (Assign (LName "x") (Op2 (Var "x") Plus (Val (IntVal 1)))) (Block [Assign (LName "y") (Val (IntVal 4))])),
        P.parse statementP "while (x) { y=4 }" ~?= Right (While (Var "x") (Block [Assign (LName "y") (Val (IntVal 4))])),
        P.parse statementP "f(a1)" ~?= Right (FunctionCallStatement (Var "f") [Var "a1"]),
        P.parse statementP "fun f(x1, x2) { y=4 }" ~?= Right (FunctionDef "f" ["x1", "x2"] (Block [Assign (LName "y") (Val (IntVal 4))])),
        P.parse statementP "return 4" ~?= Right (Return (Val (IntVal 4)))
      ]

test_all :: IO Counts
test_all = runTestTT $ TestList [test_wsP, test_stringP, test_constP, test_stringValP, test_funcValP, test_arrayValP, test_comb, test_value, test_exp, test_stat, tParseFiles]

-- >>> test_all
-- Counts {cases = 62, tried = 62, errors = 0, failures = 3}

prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

prop_roundtrip_stat :: Statement -> Bool
prop_roundtrip_stat s = P.parse statementP (pretty s) == Right s

qc :: IO ()
qc = do
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_val
  putStrLn "roundtrip_exp"
  QC.quickCheck prop_roundtrip_exp
  putStrLn "roundtrip_stat"
  QC.quickCheck prop_roundtrip_stat