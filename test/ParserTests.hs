module ParserTests where

import Control.Applicative
import LoxParser
import LoxSyntax
import ParserLib qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

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

-- pretty :: (BP.PP a) => a -> String
-- pretty = PP.render . BP.pp

-- prop_roundtrip_val :: Value -> Bool
-- prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

-- prop_roundtrip_exp :: Expression -> Bool
-- prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

-- prop_roundtrip_stat :: Statement -> Bool
-- prop_roundtrip_stat s = P.parse statementP (pretty s) == Right s

-- qc :: IO ()
-- qc = do
--   putStrLn "roundtrip_val"
--   QC.quickCheck prop_roundtrip_val
--   putStrLn "roundtrip_exp"
--   QC.quickCheck prop_roundtrip_exp
--   putStrLn "roundtrip_stat"
--   QC.quickCheck prop_roundtrip_stat
