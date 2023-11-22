module LoxParser where

import Control.Applicative
import Data.Char qualified as Char
import LoxSyntax
import ParserLib (Parser)
import ParserLib qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

-- Helper functions --
wsP :: Parser a -> Parser a
wsP p = p <* many P.space

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

-- >>> runTestTT test_wsP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

stringP :: String -> Parser ()
stringP s = wsP (P.string s) *> pure ()

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()]
    ]

-- >>> runTestTT test_stringP
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

constP :: String -> a -> Parser a
constP p x = wsP (P.string p) *> pure x

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]

-- >>> runTestTT test_constP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

-- -- >>> P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]"
-- -- Right [1,1,1]
-- brackets :: Parser a -> Parser a
-- brackets x = P.between (stringP "[") x (stringP "]")

-- valueP :: Parser Value
-- valueP = intValP <|> boolValP <|> nilValP <|> stringValP

-- -- >>> P.parse (many intValP) "1 2\n 3"
-- -- Right [IntVal 1,IntVal 2,IntVal 3]

-- intValP :: Parser Value
-- intValP = IntVal <$> wsP P.int

-- -- >>> P.parse (many boolValP) "true false\n true"
-- -- Right [BoolVal True,BoolVal False,BoolVal True]
-- boolValP :: Parser Value
-- boolValP = BoolVal <$> (constP "true" True <|> constP "false" False)

-- -- >>> P.parse (many nilValP) "nil nil\n nil"
-- -- Right [NilVal,NilVal,NilVal]
-- nilValP :: Parser Value
-- nilValP = constP "nil" NilVal

-- stringValP :: Parser Value
-- stringValP = StringVal <$> wsP (P.between (P.string "\"") (many (P.satisfy (/= '"'))) (P.string "\""))

-- test_stringValP :: Test
-- test_stringValP =
--   TestList
--     [ P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
--       P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
--       P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
--       P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
--     ]

-- -- >>> runTestTT test_stringValP
-- -- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

-- expP :: Parser Expression
-- expP = compP
--   where
--     compP = catP `P.chainl1` opAtLevel (level Gt)
--     catP = sumP `P.chainl1` opAtLevel (level Concat)
--     sumP = prodP `P.chainl1` opAtLevel (level Plus)
--     prodP = uopexpP `P.chainl1` opAtLevel (level Times)
--     uopexpP =
--       baseP
--         <|> Op1 <$> uopP <*> uopexpP
--     baseP =
--       tableConstP
--         <|> Var <$> varP
--         <|> parens expP
--         <|> Val <$> valueP

-- -- | Parse an operator at a specified precedence level
-- opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
-- opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

-- -- >>>  P.parse (many varP) "x y z"
-- -- Right [Name "x", Name "y", Name "z"]
-- -- >>> P.parse varP "(x.y[1]).z"
-- -- Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) (Val (IntVal 1)))) "z")
-- varP :: Parser Var
-- varP = mkVar <$> prefixP <*> some indexP <|> Name <$> nameP
--   where
--     mkVar :: Expression -> [Expression -> Var] -> Var
--     mkVar e l = foldr1 (\f p u -> p (Var (f u))) l e

--     prefixP :: Parser Expression
--     prefixP = parens expP <|> Var . Name <$> nameP

--     indexP :: Parser (Expression -> Var)
--     indexP =
--       flip Dot <$> (P.string "." *> nameP)
--         <|> flip Proj <$> brackets expP

-- >>> P.parse (many nameP) "x sfds _ nil"
-- Right ["x","sfds","_"]

-- nameP :: Parser Name
-- nameP = wsP $ P.filter (`notElem` LoxSyntax.reserved) ((:) <$> (P.alpha <|> P.char '_') <*> many (P.alpha <|> P.digit <|> P.char '_'))

-- -- >>> P.parse (many uopP) "- - #"
-- -- Right [Neg,Neg,Len]

-- -- >>> P.parse (many uopP) "not # - test hi"
-- -- Right [Not,Len,Neg]
-- uopP :: Parser Uop
-- uopP = constP "-" Neg <|> constP "#" Len <|> constP "not" Not

-- -- >>> P.parse (many bopP) "+ >= .."
-- -- Right [Plus,Ge,Concat]

-- -- >>> P.parse (many bopP) "- * // % == >= > <= < .."
-- -- Right [Minus,Times,Divide,Modulo,Eq,Ge,Gt,Le,Lt,Concat]
-- bopP :: Parser Bop
-- bopP = constP "+" Plus <|> constP "-" Minus <|> constP "*" Times <|> constP "//" Divide <|> constP "%" Modulo <|> constP "==" Eq <|> constP ">=" Ge <|> constP ">" Gt <|> constP "<=" Le <|> constP "<" Lt <|> constP ".." Concat

-- -- >>> P.parse tableConstP "{ x = 2, [3] = false }"
-- -- Right (TableConst [FieldName "x" (Val (IntVal 2)),FieldKey (Val (IntVal 3)) (Val (BoolVal False))])
-- tableConstP :: Parser Expression
-- tableConstP = TableConst <$> braces (P.sepBy fieldP (stringP ","))
--   where
--     fieldP = FieldName <$> nameP <*> (stringP "=" *> expP) <|> FieldKey <$> brackets expP <*> (stringP "=" *> expP)

-- statementP :: Parser Statement
-- statementP = assignP <|> ifP <|> whileP <|> emptyP <|> repeatP
--   where
--     assignP = Assign <$> varP <*> (stringP "=" *> expP)
--     ifP = If <$> (stringP "if" *> expP) <*> (stringP "then" *> blockP) <*> (stringP "else" *> blockP <* stringP "end")
--     whileP = While <$> (stringP "while" *> expP) <*> (stringP "do" *> blockP <* stringP "end")
--     emptyP = constP ";" Empty
--     repeatP = Repeat <$> (stringP "repeat" *> blockP) <*> (stringP "until" *> expP)

-- blockP :: Parser Block
-- blockP = wsP $ Block <$> many statementP

-- parseLuExp :: String -> Either P.ParseError Expression
-- parseLuExp = P.parse expP

-- parseLuStat :: String -> Either P.ParseError Statement
-- parseLuStat = P.parse statementP

-- parseLuFile :: String -> IO (Either P.ParseError Block)
-- parseLuFile = P.parseFromFile (const <$> blockP <*> P.eof)

-- tParseFiles :: Test
-- tParseFiles =
--   "parse files"
--     ~: TestList
--       [ "fact" ~: p "lu/fact.lu" wFact,
--         "test" ~: p "lu/test.lu" wTest,
--         "abs" ~: p "lu/abs.lu" wAbs,
--         "times" ~: p "lu/times.lu" wTimes,
--         "table" ~: p "lu/table.lu" wTable,
--         "bfs" ~: p "lu/bfs.lu" wBfs
--       ]
--   where
--     p fn ast = do
--       result <- parseLuFile fn
--       case result of
--         (Left _) -> assert False
--         (Right ast') -> assert (ast == ast')

-- test_comb =
--   "parsing combinators"
--     ~: TestList
--       [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
--         P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc",
--         P.parse (stringP "a") "a" ~?= Right (),
--         P.parse (stringP "a") "b" ~?= Left "No parses",
--         P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
--         P.parse (constP "&" 'a') "&  " ~?= Right 'a',
--         P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
--         P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]" ~?= Right [1, 1, 1]
--       ]

-- test_value =
--   "parsing values"
--     ~: TestList
--       [ P.parse (many intValP) "1 2\n 3" ~?= Right [IntVal 1, IntVal 2, IntVal 3],
--         P.parse (many boolValP) "true false\n true" ~?= Right [BoolVal True, BoolVal False, BoolVal True],
--         P.parse (many nilValP) "nil nil\n nil" ~?= Right [NilVal, NilVal, NilVal],
--         P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
--         P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
--         P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
--         P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
--       ]

-- test_exp =
--   "parsing expressions"
--     ~: TestList
--       [ P.parse (many varP) "x y z" ~?= Right [Name "x", Name "y", Name "z"],
--         P.parse varP "(x.y[1]).z" ~?= Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) (Val (IntVal 1)))) "z"),
--         P.parse (many nameP) "x sfds _ nil" ~?= Right ["x", "sfds", "_"],
--         P.parse (many uopP) "- - #" ~?= Right [Neg, Neg, Len],
--         P.parse (many bopP) "+ >= .." ~?= Right [Plus, Ge, Concat],
--         P.parse tableConstP "{ x = 2, [3] = false }"
--           ~?= Right (TableConst [FieldName "x" (Val (IntVal 2)), FieldKey (Val (IntVal 3)) (Val (BoolVal False))])
--       ]

-- test_stat =
--   "parsing statements"
--     ~: TestList
--       [ P.parse statementP ";" ~?= Right Empty,
--         P.parse statementP "x=3" ~?= Right (Assign (Name "x") (Val (IntVal 3))),
--         P.parse statementP "if x then y=nil else end"
--           ~?= Right (If (Var (Name "x")) (Block [Assign (Name "y") (Val NilVal)]) (Block [])),
--         P.parse statementP "while nil do end"
--           ~?= Right (While (Val NilVal) (Block [])),
--         P.parse statementP "repeat ; ; until false"
--           ~?= Right (Repeat (Block [Empty, Empty]) (Val (BoolVal False)))
--       ]

-- -- >>> runTestTT test_stat
-- -- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

-- test_all :: IO Counts
-- test_all = runTestTT $ TestList [test_comb, test_value, test_exp, test_stat, tParseFiles]

-- -- >>> test_all
-- -- Counts {cases = 32, tried = 32, errors = 0, failures = 0}

-- qc :: IO ()
-- qc = do
--   putStrLn "roundtrip_val"
--   QC.quickCheck prop_roundtrip_val
--   putStrLn "roundtrip_exp"
--   QC.quickCheck prop_roundtrip_exp
--   putStrLn "roundtrip_stat"
--   QC.quickCheck prop_roundtrip_stat