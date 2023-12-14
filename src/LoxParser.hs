module LoxParser where

import Control.Applicative
import Data.Char qualified as Char
import GHC.Arr (array)
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

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

-- Basic parsers --
valueP :: Parser Value
valueP = intValP <|> boolValP <|> nilValP <|> stringValP <|> arrayValP <|> funcValP

intValP :: Parser Value
intValP = IntVal <$> wsP P.int

boolValP :: Parser Value
boolValP = BoolVal <$> (constP "true" True <|> constP "false" False)

nilValP :: Parser Value
nilValP = constP "nil" NilVal

stringValP :: Parser Value
stringValP = StringVal <$> wsP (P.between (P.string "\"") (many (P.satisfy (/= '"'))) (P.string "\""))

arrayValP :: Parser Value
arrayValP = ArrayVal <$> brackets (P.sepBy valueP (stringP ","))

funcValP :: Parser Value
funcValP = FunctionValIncomplete <$> (stringP "\\" *> parens (P.sepBy varP (stringP ","))) <*> braces blockP

-- Expression parser --
expP :: Parser Expression
expP = orP
  where
    orP = andP `P.chainl1` opAtLevel (level Or)
    andP = equalityP `P.chainl1` opAtLevel (level And)
    equalityP = compP `P.chainl1` opAtLevel (level Eq)
    compP = sumP `P.chainl1` opAtLevel (level Lt)
    sumP = timesP `P.chainl1` opAtLevel (level Plus)
    timesP = funcCallP `P.chainl1` opAtLevel (level Times)
    funcCallP = funcCallExpP arrayIndexP
    arrayIndexP = arrayIndexExpP uopexpP
    uopexpP =
      baseP
        <|> Op1 <$> uopP <*> uopexpP
    baseP =
      arrayConsP
        <|> parens expP -- Supports Grouping in Lox
        <|> Var <$> varP
        <|> Val <$> valueP

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
funcArgsExpP = wsP (parens $ P.sepBy expP (wsP $ stringP ","))

arrayIndexExpP :: Parser Expression -> Parser Expression
arrayIndexExpP p = process <$> first <*> rest
  where
    process :: Expression -> [Expression] -> Expression
    process = foldl comb

    comb :: Expression -> Expression -> Expression
    comb = ArrayIndex

    first :: Parser Expression
    first = p

    rest :: Parser [Expression]
    rest = many $ brackets expP

arrayConsP :: Parser Expression
arrayConsP = ArrayCons <$> brackets (P.sepBy expP (stringP ","))

-- Statement parser --
statementP :: Parser Statement
statementP = assignP <|> varDecP <|> ifP <|> whileP <|> forP <|> funcCallStatP <|> funcDefP <|> returnP <|> emptyP

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
funcDefP = FunctionDef <$> (stringP "fun" *> varP) <*> parens (P.sepBy varP (stringP ",")) <*> braces blockP

-- return e
returnP :: Parser Statement
returnP = Return <$> (stringP "return" *> expP)

-- // or ;
emptyP :: Parser Statement
emptyP = (wsP (stringP "//" *> many (P.satisfy (/= '\n'))) *> pure Empty) <|> constP ";" Empty

-- Block parser --
blockP :: Parser Block
blockP = wsP $ Block <$> many statementP

parseLoxExp :: String -> Either P.ParseError Expression
parseLoxExp = P.parse expP

parseLoxStat :: String -> Either P.ParseError Statement
parseLoxStat = P.parse statementP

parseLoxFile :: String -> IO (Either P.ParseError Block)
parseLoxFile = P.parseFromFile (const <$> blockP <*> P.eof)