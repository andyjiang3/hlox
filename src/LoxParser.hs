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

stringP :: String -> Parser ()
stringP s = wsP (P.string s) *> pure ()

constP :: String -> a -> Parser a
constP p x = wsP (P.string p) *> pure x

-- removeSpacesAround :: Parser a -> Parser a
-- removeSpacesAround p = many P.space *> p <* many P.space

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

-- Basic parsers --
valueP :: Parser Value
valueP = intValP <|> boolValP <|> nilValP <|> stringValP

intValP :: Parser Value
intValP = IntVal <$> wsP P.int

boolValP :: Parser Value
boolValP = BoolVal <$> (constP "true" True <|> constP "false" False)

nilValP :: Parser Value
nilValP = constP "nil" NilVal

stringValP :: Parser Value
stringValP = StringVal <$> wsP (P.between (P.string "\"") (many (P.satisfy (/= '"'))) (P.string "\""))

-- Expression parser --
expP :: Parser Expression
expP = orP
  where
    orP = andP `P.chainl1` opAtLevel (level Or)
    andP = equalityP `P.chainl1` opAtLevel (level And)
    equalityP = compP `P.chainl1` opAtLevel (level Eq)
    compP = sumP `P.chainl1` opAtLevel (level Lt)
    sumP = timesP `P.chainl1` opAtLevel (level Plus)
    timesP = uopexpP `P.chainl1` opAtLevel (level Times)
    funcCallP = funcCallExpP uopexpP -- TODO: support array index
    uopexpP =
      baseP
        <|> Op1 <$> uopP <*> uopexpP
    baseP =
      parens expP -- Supports Grouping in Lox -- TODO: support array
        <|> Var <$> varP
        <|> Val <$> valueP

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

varP :: Parser Name
varP = wsP $ P.filter (`notElem` LoxSyntax.reserved) ((:) <$> (P.alpha <|> P.char '_') <*> many (P.alpha <|> P.digit <|> P.char '_'))

uopP :: Parser Uop
uopP = constP "-" Neg <|> constP "!" Not

bopP :: Parser Bop
bopP =
  constP "+" Plus
    <|> constP "-" Minus
    <|> constP "*" Times
    <|> constP "/" Divide
    <|> constP "%" Modulo
    <|> constP "or" Or
    <|> constP "and" And
    <|> constP "==" Eq
    <|> constP "!=" Ne
    <|> constP ">=" Ge
    <|> constP ">" Gt
    <|> constP "<=" Le
    <|> constP "<" Lt

funcCallExpP :: Parser Expression -> Parser Expression
funcCallExpP p = process <$> first <*> rest
  where
    process :: Expression -> [[Expression]] -> Expression
    process = foldl comb

    comb :: Expression -> [Expression] -> Expression
    comb = FunctionCall

    first :: Parser Expression
    first = p

    rest :: Parser [[Expression]]
    rest = many funcArgsExpP

funcArgsExpP :: Parser [Expression]
funcArgsExpP = parens $ P.sepBy expP (wsP $ stringP ",") -- TODO: should I trim both sides or is right side enough

-- Statement parser --
statementP :: Parser Statement
statementP = assignP <|> ifP <|> whileP <|> forP <|> funcCallStatP <|> funcDefP <|> returnP <|> printP <|> emptyP

varLValueP :: Parser LValue
varLValueP = process <$> first <*> rest
  where
    process :: LValue -> [Expression] -> LValue
    process = foldl comb

    comb :: LValue -> Expression -> LValue
    comb = LArrayIndex

    first :: Parser LValue
    first = LName <$> varP

    rest :: Parser [Expression]
    rest = many $ brackets expP

assignP :: Parser Statement
assignP = Assign <$> varLValueP <*> (stringP "=" *> expP)

varDecP = VarDecl <$> varP <*> (stringP "=" *> expP)

-- if (e) { s1 } else { s2 }
ifP :: Parser Statement
ifP = If <$> (stringP "if" *> parens expP) <*> braces blockP <*> (stringP "else" *> braces blockP)

-- while (e) { s }
whileP :: Parser Statement
whileP = While <$> (stringP "while" *> parens expP) <*> braces blockP

-- for (var x = e; e; e) { s }
forP :: Parser Statement
forP = For <$> (stringP "for" *> stringP "(" *> varDecP) <*> (stringP ";" *> expP) <*> (stringP ";" *> expP) <*> (stringP ")" *> braces blockP)

-- f(e1, ..., en)
funcCallStatP :: Parser Statement
funcCallStatP = convertToStat <$> funcCallExpP expP
  where
    convertToStat :: Expression -> Statement
    convertToStat (FunctionCall e es) = FunctionCallStatement e es
    convertToStat _ = error "Bug: convertToStatement should only be called on FunctionCall"

-- fun f(x1, ..., xn) { s }
funcDefP :: Parser Statement
funcDefP = FunctionDef <$> (stringP "fun" *> expP) <*> parens (P.sepBy varP (stringP ",")) <*> braces blockP

-- return e
returnP :: Parser Statement
returnP = Return <$> (stringP "return" *> expP)

-- print e
printP :: Parser Statement
printP = Print <$> (stringP "print" *> expP)

-- ;
emptyP :: Parser Statement
emptyP = constP ";" Empty

-- Block parser --
blockP :: Parser Block
blockP = wsP $ Block <$> many statementP

parseLuExp :: String -> Either P.ParseError Expression
parseLuExp = P.parse expP

parseLuStat :: String -> Either P.ParseError Statement
parseLuStat = P.parse statementP

parseLuFile :: String -> IO (Either P.ParseError Block)
parseLuFile = P.parseFromFile (const <$> blockP <*> P.eof)

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
        P.parse (funcCallExpP expP) "f(a1)" ~?= Right (FunctionCall (Var "f") [Var "a1"])
      ]

-- test_stat =
--   "parsing statements"
--     ~: TestList
--       [ P.parse statementP ";" ~?= Right Empty,
--         P.parse statementP "var x=3" ~?= Right (Assign (LName "x") (Val (IntVal 3))),
--         P.parse statementP "var x=3" ~?= Right (Assign (LName "x") (Val (IntVal 3))),
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
