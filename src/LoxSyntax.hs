module LoxSyntax where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.List (intersperse)
import Data.Map (Map)
import Data.Map qualified as Map
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

newtype Block = Block [Statement] -- s1 ... sn
  deriving (Eq, Show, Ord)

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

type Name = String -- name of a variable

-- produce an effect
data Statement
  = Assign LValue Expression -- x = e, a[i] = e
  | VarDecl Name Expression -- var x = e
  | If Expression Block Block -- if (e) { s1 } else { s2 }
  | While Expression Block -- while (e) { s }
  | For Statement Expression Statement Block -- for (var x = e; e; e) { s }
  | FunctionCallStatement Expression [Expression] -- f(e1, ..., en)
  | FunctionDef Name [Name] Block -- fun f(x1, ..., xn) { s }
  | Return Expression -- return e
  | EndStatement -- only used internally
  | Empty -- ';'
  deriving (Eq, Show, Ord)

-- produce a value, can be assigned or used as an operand
data Expression
  = Var Name -- global variables x
  | Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  | ArrayIndex Expression Expression -- a[i]
  | ArrayCons [Expression] -- [e1, ..., en]
  | FunctionCall Expression [Expression] -- f(e1, ..., en)
  deriving (Eq, Show, Ord)

data LValue
  = LName Name -- x, global variable
  | LArrayIndex LValue Expression -- a[i], a[i][j]
  deriving (Eq, Show, Ord)

data Value -- literals
  = NilVal -- nil
  | IntVal Int -- 1
  | BoolVal Bool -- false, true
  | StringVal String -- "abd"
  | ArrayVal [Value] -- [v1, ..., vn]
  | FunctionValIncomplete [Name] Block -- for internal use only
  | ErrorVal String -- raise an error
  | FunctionVal [Name] Block Id -- \(x1, ..., xn) { s }, supports anonymous function
  deriving
    ( Eq,
      Show,
      Ord
    )

data Uop
  = Neg -- `-` :: Int -> Int
  | Not -- `!` :: a -> Bool
  deriving (Eq, Show, Enum, Bounded, Ord)

data Bop
  = Plus -- `+`  :: Int -> Int -> Int
  | Minus -- `-`  :: Int -> Int -> Int
  | Times -- `*`  :: Int -> Int -> Int
  | Divide -- `/` :: Int -> Int -> Int   -- floor division
  | Modulo -- `%`  :: Int -> Int -> Int
  | Or -- `or` :: Bool -> Bool -> Bool
  | And -- `and` :: Bool -> Bool -> Bool
  | Eq -- `==` :: a -> a -> Bool
  | Ne -- `!=` :: a -> a -> Bool
  | Gt -- `>`  :: a -> a -> Bool
  | Ge -- `>=` :: a -> a -> Bool
  | Lt -- `<`  :: a -> a -> Bool
  | Le -- `<=` :: a -> a -> Bool
  deriving (Eq, Show, Enum, Bounded, Ord)

reserved :: [String]
reserved =
  [ "and",
    "class", -- for class
    "else",
    "else if",
    "false",
    "for",
    "fun",
    "if",
    "nil",
    "or",
    "print",
    "return",
    "super", -- for class
    "this", -- for class
    "true",
    "var",
    "while",
    "print"
  ]

level :: Bop -> Int
level Times = 13
level Divide = 13
level Modulo = 13
level Plus = 11
level Minus = 11
level Gt = 7
level Ge = 7
level Lt = 7
level Le = 7
level Eq = 5
level Ne = 5
level And = 4
level Or = 3

-- 1_abs.lox
loxAbs :: Block
loxAbs =
  Block [VarDecl "x" (Op2 (Val (IntVal 0)) Minus (Val (IntVal 3))), If (Op2 (Var "x") Lt (Val (IntVal 0))) (Block [Assign (LName "x") (Op2 (Val (IntVal 0)) Minus (Var "x"))]) (Block [])]

-- 2_exp.lox
loxExp :: Block
loxExp =
  Block [VarDecl "x1" (Op2 (Val (IntVal 1)) Plus (Val (IntVal 3))), VarDecl "x2" (Op2 (Val (IntVal 1)) Plus (Val NilVal)), VarDecl "x3" (Op2 (Val (IntVal 1)) Divide (Val (IntVal 0))), VarDecl "x4" (Op2 (Val (IntVal 1)) Plus (Val (StringVal "s"))), VarDecl "x5" (Op2 (Val (IntVal 1)) Lt (Val (BoolVal True))), VarDecl "x6" (Op1 Not (Val (IntVal 1))), VarDecl "x7" (Op2 (Val NilVal) Eq (Val (BoolVal True)))]

-- 3_scope.lox
loxScope :: Block
loxScope =
  Block [VarDecl "x" (Val (IntVal 0)), VarDecl "y" (Val (IntVal 0)), If (Val (BoolVal True)) (Block [VarDecl "x" (Val (IntVal 10)), Assign (LName "y") (Val (IntVal 10))]) (Block []), VarDecl "z" (Op2 (Op2 (Val (IntVal 12)) Times (Val (IntVal 3))) Plus (Val (IntVal 4)))]

-- 4_func.lox
loxAdvFunc :: Block
loxAdvFunc =
  Block [VarDecl "x" (Val (IntVal 1)), VarDecl "y" (Val (IntVal 2)), FunctionDef "t" ["z"] (Block [Assign (LName "x") (Op2 (Var "x") Plus (Val (IntVal 1))), Return (Op2 (Var "z") Plus (Val (IntVal 6)))]), VarDecl "z" (Op2 (FunctionCall (Var "t") [Var "y"]) Plus (Val (IntVal 1))), Empty]

-- 5_anon_func.lox
loxAnonFunc :: Block
loxAnonFunc = Block [FunctionDef "test" ["a", "b"] (Block [Return (FunctionCall (Var "a") [Var "b"])]), VarDecl "x" (FunctionCall (Var "test") [Val (FunctionValIncomplete ["y"] (Block [Return (Op2 (Var "y") Plus (Val (IntVal 1)))])), Val (IntVal 1)])]

-- 6_closure.lox
loxClosure :: Block
loxClosure = Block [VarDecl "outside" (Val (IntVal 7)), FunctionDef "f2" [] (Block [Return (Var "outside")]), FunctionDef "outer" [] (Block [VarDecl "outside" (Val (IntVal 5)), FunctionDef "inner" [] (Block [Assign (LName "outside") (Op2 (Var "outside") Plus (Val (IntVal 1))), Return (Var "outside")]), Return (Var "inner")]), VarDecl "t" (FunctionCall (Var "outer") []), VarDecl "x" (FunctionCall (Var "t") []), Empty]

-- 7_array.lox
loxArray :: Block
loxArray = Block [VarDecl "x" (ArrayCons [Val (IntVal 1), Val (IntVal 2), Val (IntVal 3)]), Assign (LArrayIndex (LName "x") (Val (IntVal 1))) (Val (IntVal 0)), VarDecl "y" (Op2 (ArrayIndex (Var "x") (Val (IntVal 1))) Plus (ArrayIndex (Var "x") (Val (IntVal 2)))), VarDecl "z" (ArrayCons [ArrayCons [Val (IntVal 1), Val (IntVal 2)], ArrayCons [Val (IntVal 2), Val (IntVal 3)]]), Assign (LArrayIndex (LArrayIndex (LName "z") (Val (IntVal 0))) (Val (IntVal 1))) (Val (IntVal 4))]

-- 8_first_class_func.lox
loxFstClassFunc :: Block
loxFstClassFunc =
  Block [FunctionDef "addSub" ["x"] (Block [FunctionDef "add" ["x"] (Block [Return (Op2 (Var "x") Plus (Val (IntVal 1)))]), FunctionDef "sub" ["x"] (Block [Return (Op2 (Var "x") Minus (Val (IntVal 1)))]), Return (FunctionCall (Var "sub") [FunctionCall (Var "add") [Var "x"]])]), VarDecl "x" (Op2 (FunctionCall (Var "addSub") [Val (IntVal 1)]) Plus (FunctionCall (Var "addSub") [Val (IntVal 1)])), Empty]

-- 9_more_closure.lox
loxMoreClosure :: Block
loxMoreClosure =
  Block [VarDecl "outside" (Val (IntVal 10)), FunctionDef "outer" [] (Block [VarDecl "outside" (Val (IntVal 0)), FunctionDef "inner" [] (Block [Assign (LName "outside") (Op2 (Var "outside") Plus (Val (IntVal 1))), Return (Var "outside")]), Return (Var "inner")]), VarDecl "x" (ArrayCons [FunctionCall (Var "outer") [], FunctionCall (Var "outer") []]), FunctionCallStatement (ArrayIndex (Var "x") (Val (IntVal 0))) [], Empty, FunctionCallStatement (ArrayIndex (Var "x") (Val (IntVal 0))) [], Empty, VarDecl "y" (FunctionCall (ArrayIndex (Var "x") (Val (IntVal 0))) []), Empty, VarDecl "z" (FunctionCall (ArrayIndex (Var "x") (Val (IntVal 1))) []), Empty]

-- 10_recursion.lox
loxRecursion :: Block
loxRecursion =
  Block [FunctionDef "fib" ["n"] (Block [If (Op2 (Var "n") Le (Val (IntVal 1))) (Block [Return (Var "n")]) (Block [Return (Op2 (FunctionCall (Var "fib") [Op2 (Var "n") Minus (Val (IntVal 1))]) Plus (FunctionCall (Var "fib") [Op2 (Var "n") Minus (Val (IntVal 2))]))])]), VarDecl "x" (FunctionCall (Var "fib") [Val (IntVal 5)])]

loxAssignError :: Block
loxAssignError = Block [VarDecl "x" (Val (IntVal 10)), Assign (LName "y") (Val (IntVal 5)), VarDecl "z" (Val (IntVal 10))]

loxDeclareError :: Block
loxDeclareError = Block [VarDecl "x" (Val (IntVal 10)),VarDecl "y" (Val (IntVal 10)),VarDecl "x" (Val (IntVal 10))]

loxFunError :: Block
loxFunError = Block [FunctionDef "f1" [] (Block [Return (Val (IntVal 0))]), VarDecl "y" (FunctionCall (Var "f2") [Val (IntVal 10)])]

type Id = Int

class PP a where
  pp :: a -> Doc

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: (PP a) => a -> String
pretty = PP.render . pp

-- | Compact version. Displays its argument without newlines.
-- oneLine :: (PP a) => a -> String
-- oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp
instance PP Uop where
  pp Neg = PP.char '-'
  pp Not = PP.text "!"

instance PP Bool where
  pp True = PP.text "true"
  pp False = PP.text "false"

instance PP String where
  pp = PP.text

instance PP Int where
  pp = PP.int

instance PP LValue where
  pp (LName n) = PP.text n
  pp (LArrayIndex e1 e2) = pp e1 <> PP.brackets (pp e2)

instance PP Value where
  pp (IntVal i) = pp i
  pp (BoolVal b) = pp b
  pp NilVal = PP.text "nil"
  pp (StringVal s) = PP.text ("\"" <> s <> "\"")
  pp (FunctionVal n blk _) = parens (commaSep (map pp n)) <+> PP.text "{" <+> pp blk <+> PP.text "}"
    where
      parens d = PP.text "\\(" <> d <> PP.text ")"
      commaSep = foldr (<+>) (PP.text "") . intersperse (PP.text ",")
  pp (FunctionValIncomplete n blk) = parens (commaSep (map pp n)) <+> PP.text "{" <+> pp blk <+> PP.text "}"
    where
      parens d = PP.text "\\(" <> d <> PP.text ")"
      commaSep = foldr (<+>) (PP.text "") . intersperse (PP.text ",")
  pp (ArrayVal vs) = PP.brackets (commaSep (map pp vs))
    where
      commaSep = foldr (<+>) (PP.text "") . intersperse (PP.text ",")
  pp (ErrorVal e) = PP.text e

isBase :: Expression -> Bool
isBase Val {} = True
isBase Var {} = True
isBase Op1 {} = True
isBase _ = False

instance PP Bop where
  pp Plus = PP.char '+'
  pp Minus = PP.char '-'
  pp Times = PP.char '*'
  pp Divide = PP.text "/"
  pp Modulo = PP.text "%"
  pp Gt = PP.char '>'
  pp Ge = PP.text ">="
  pp Lt = PP.char '<'
  pp Le = PP.text "<="
  pp Eq = PP.text "=="
  pp Ne = PP.text "!="
  pp Or = PP.text "or"
  pp And = PP.text "and"

instance PP Expression where
  pp (Var v) = pp v
  pp (Val v) = pp v
  pp (Op1 o v) = pp o <+> if isBase v then pp v else PP.parens (pp v)
  pp e@Op2 {} = ppPrec 0 e
    where
      ppPrec n (Op2 e1 bop e2) =
        ppParens (level bop < n) $
          ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
      ppPrec _ e' = pp e'
      ppParens b = if b then PP.parens else id
  pp (FunctionCall name args) = pp name <+> parens (commaSep (map pp args))
    where
      parens d = PP.text "(" <> d <> PP.text ")"
      commaSep = foldr (<+>) (PP.text "") . intersperse (PP.text ",")
  pp (ArrayIndex e1 e2) = ppArrayName e1 <> PP.brackets (pp e2)
    where
      ppArrayName (Var v) = PP.text v
      ppArrayName e1 = PP.parens (pp e1)
  pp (ArrayCons es) = PP.brackets (commaSep (map pp es))
    where
      commaSep = foldr (<+>) (PP.text "") . intersperse (PP.text ",")

instance PP Block where
  pp (Block [s]) = pp s
  pp (Block ss) = PP.vcat (map pp ss)

ppSS :: [Statement] -> Doc
ppSS ss = PP.vcat (map pp ss)

instance PP Statement where
  pp (Assign x e) = pp x <+> PP.equals <+> pp e
  pp (If guard b1 b2) =
    PP.hang (PP.text "if" <+> PP.parens (pp guard) <+> PP.text "{") 2 (pp b1)
      PP.$$ PP.nest 2 (PP.text "} else {" PP.$$ pp b2)
      PP.$$ PP.text "}"
  pp (While guard e) =
    PP.hang (PP.text "while" <+> PP.parens (pp guard) <+> PP.text "{") 2 (pp e)
      PP.$+$ PP.text "}"
  pp (Return x) = PP.text "return" <+> pp x
  pp Empty = PP.semi
  pp EndStatement = PP.semi
  pp (VarDecl x e) = PP.text "var" <+> pp x <+> PP.equals <+> pp e
  pp (FunctionCallStatement name args) = ppFuncName name <+> PP.parens (commaSep (map pp args))
    where
      commaSep = foldr (<+>) (PP.text "") . intersperse (PP.text ",")
      ppFuncName (Var v) = PP.text v
      ppFuncName e1 = PP.parens (pp e1)
  pp (FunctionDef name params block) =
    PP.hang (PP.text "fun" <+> pp name <+> PP.parens (commaSep (map pp params)) <+> PP.text "{") 2 (PP.nest 4 (pp block)) <+> PP.text "}"
    where
      commaSep = foldr (<+>) (PP.text "") . intersperse (PP.text ",")
  pp (For s1 e s2 b) =
    PP.hang (PP.text "for" <+> PP.parens (pp s1 <+> PP.semi <+> pp e <+> PP.semi <+> pp s2) <+> PP.text "{") 2 (pp b)
      PP.$+$ PP.text "}"

instance (PP a) => PP (Map Value a) where
  pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
    where
      ppa (StringVal s, v2) = PP.text s <+> PP.text "=" <+> pp v2
      ppa (v1, v2) = PP.brackets (pp v1) <+> PP.text "=" <+> pp v2

instance (PP a) => PP (Map Name a) where
  pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
    where
      ppa (s, v2) = PP.text s <+> PP.text "=" <+> pp v2
