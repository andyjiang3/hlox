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
statementP = assignP <|> varDecP <|> ifP <|> whileP <|> forP <|> funcCallStatP <|> funcDefP <|> returnP <|> printP <|> emptyP

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

varDecP = VarDecl <$> (stringP "var" *> varP) <*> (stringP "=" *> expP)

-- if (e) { s1 } else { s2 }
ifP :: Parser Statement
ifP = If <$> (stringP "if" *> parens expP) <*> braces blockP <*> (stringP "else" *> braces blockP <|> pure (Block []))

-- while (e) { s }
whileP :: Parser Statement
whileP = While <$> (stringP "while" *> parens expP) <*> braces blockP

-- for (var x = e; e; e) { s }
forP :: Parser Statement
forP = For <$> (stringP "for" *> stringP "(" *> varDecP) <*> (stringP ";" *> expP) <*> (stringP ";" *> statementP) <*> (stringP ")" *> braces blockP)

-- f(e1, ..., en)
funcCallStatP :: Parser Statement
funcCallStatP = convertToStat <$> funcCallExpP expP
  where
    convertToStat :: Expression -> Statement
    convertToStat (FunctionCall e es) = FunctionCallStatement e es
    convertToStat _ = error "Err: convertToStatement should only be called on FunctionCall"

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

tParseFiles :: Test
tParseFiles =
  "parse files"
    ~: TestList
      [ "hello world" ~: p "test/programs/hello_world.lox" loxTest,
        "abs" ~: p "test/programs/abs.lox" loxAbs,
        "exp" ~: p "test/programs/exp.lox" loxExp,
        "basic func" ~: p "test/programs/basic_func.lox" loxBasicFunc
      ]
  where
    p fn ast = do
      result <- parseLuFile fn
      case result of
        (Left _) -> assert False
        (Right ast') -> assert (ast == ast')

-- hello_world.lox
loxTest :: Block
loxTest =
  Block
    [ Print (Val (StringVal "Hello, world!"))
    ]

-- abs.lox
loxAbs :: Block
loxAbs =
  Block
    [ VarDecl "x" (Op2 (Val (IntVal 0)) Minus (Val (IntVal 3))),
      If
        (Op2 (Var "x") Lt (Val (IntVal 0)))
        (Block [Assign (LName "x") (Op2 (Val (IntVal 0)) Minus (Var "x"))])
        (Block [])
    ]

-- exp.lox
loxExp :: Block
loxExp =
  Block
    [ VarDecl "x1" (Op2 (Val (IntVal 1)) Plus (Val (IntVal 3))),
      VarDecl "x2" (Op2 (Val (IntVal 1)) Plus (Val NilVal)),
      VarDecl "x3" (Op2 (Val (IntVal 1)) Divide (Val (IntVal 0))),
      VarDecl "x4" (Op2 (Val (IntVal 1)) Plus (Val (StringVal "s"))),
      VarDecl "x5" (Op2 (Val (IntVal 1)) Lt (Val (BoolVal True))),
      VarDecl "x6" (Op1 Not (Val (IntVal 1))),
      VarDecl "x7" (Op2 (Val NilVal) Eq (Val (BoolVal True)))
    ]

-- basic_func.lox
loxBasicFunc :: Block
loxBasicFunc =
  Block
    [ VarDecl "x" (Val (IntVal 1)),
      VarDecl "y" (Val (IntVal 2)),
      FunctionDef (Var "t") ["z"] (Block [Assign (LName "x") (Op2 (Var "x") Plus (Val (IntVal 1))), Return (Var "z")]),
      FunctionCallStatement (Var "t") [Var "y"]
    ]

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
        P.parse (funcCallExpP expP) "f(a1, a2)" ~?= Right (FunctionCall (Var "f") [Var "a1", Var "a2"])
      ]

test_stat =
  "parsing statements"
    ~: TestList
      [ P.parse statementP ";" ~?= Right Empty,
        P.parse statementP "y=4" ~?= Right (Assign (LName "y") (Val (IntVal 4))),
        P.parse statementP "var x=3" ~?= Right (VarDecl "x" (Val (IntVal 3))), -- TODO: val x=3 weird error
        P.parse statementP "if (x) { y=4 } else { y=5 }" ~?= Right (If (Var "x") (Block [Assign (LName "y") (Val (IntVal 4))]) (Block [Assign (LName "y") (Val (IntVal 5))])),
        P.parse statementP "for (var x=3; x<6; x=x+1) { y=4 }" ~?= Right (For (VarDecl "x" (Val (IntVal 3))) (Op2 (Var "x") Lt (Val (IntVal 6))) (Assign (LName "x") (Op2 (Var "x") Plus (Val (IntVal 1)))) (Block [Assign (LName "y") (Val (IntVal 4))])),
        P.parse statementP "while (x) { y=4 }" ~?= Right (While (Var "x") (Block [Assign (LName "y") (Val (IntVal 4))])),
        P.parse statementP "f(a1)" ~?= Right (FunctionCallStatement (Var "f") [Var "a1"]),
        P.parse statementP "fun f(x1, x2) { y=4 }" ~?= Right (FunctionDef (Var "f") ["x1", "x2"] (Block [Assign (LName "y") (Val (IntVal 4))])),
        P.parse statementP "return 4" ~?= Right (Return (Val (IntVal 4))),
        P.parse statementP "print 4" ~?= Right (Print (Val (IntVal 4)))
      ]

test_all :: IO Counts
test_all = runTestTT $ TestList [test_comb, test_value, test_exp, test_stat, tParseFiles]

-- >>> test_all
-- Counts {cases = 37, tried = 37, errors = 0, failures = 0}