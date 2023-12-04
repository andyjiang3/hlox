module LoxSyntax where

import Control.Monad (mapM_)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import Test.HUnit
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))

newtype Block = Block [Statement] -- s1 ... sn
  deriving (Eq, Show, Ord)

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

type Name = String -- name of a variable

-- produce an effect
data Statement
  = Assign LValue Expression -- x = e, a[i] = e, TODO: support
  | VarDecl Name Expression -- var x = e
  | If Expression Block Block -- if (e) { s1 } else { s2 }
  | While Expression Block -- while (e) { s }
  | For Statement Expression Statement Block -- for (var x = e; e; e) { s }
  | FunctionCallStatement Expression [Expression] -- f(e1, ..., en), TODO: Name instead of expresssion?
  | FunctionDef Name [Name] Block -- fun f(x1, ..., xn) { s }
  | Return Expression -- return e
  | Print Expression -- print e
  | Empty -- ';'
  deriving (Eq, Show, Ord)

-- produce a value, can be assigned or used as an operand
data Expression
  = Var Name -- global variables x
  | Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  | -- TODO: Support array, a[i] and [e1, ..., en]
    FunctionCall Expression [Expression] -- f(e1, ..., en)
  deriving (Eq, Show, Ord)

data LValue
  = LName Name -- x, global variable
  | LArrayIndex LValue Expression -- a[i], a[i][j], TODO: support
  deriving (Eq, Show, Ord)

data Value -- literals
  = NilVal -- nil
  | IntVal Int -- 1
  | BoolVal Bool -- false, true
  | StringVal String -- "abd"
  | FunctionVal [Name] Block -- (x1, ..., xn) { s }
  deriving
    ( Eq,
      Show,
      Ord
    )

-- | FunctionVal [Name] Block -- function (x1, ..., xn) s end, todo: constraint name more?
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
    "else if", -- TODO: figure out edge case with spacing
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
    "print" -- do we like this
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
      FunctionDef "t" ["z"] (Block [Assign (LName "x") (Op2 (Var "x") Plus (Val (IntVal 1))), Return (Var "z")]),
      FunctionCallStatement (Var "t") [Var "y"]
    ]

-- adv_func.lox
loxAdvFunc :: Block
loxAdvFunc =
  Block
    [ VarDecl "x" (Val (IntVal 1)),
      VarDecl "y" (Val (IntVal 2)),
      FunctionDef "t" ["z"] (Block [Assign (LName "x") (Op2 (Var "x") Plus (Val (IntVal 1))), Return (Var "z")]),
      VarDecl "z" (FunctionCall (Var "t") [Var "y"])
    ]