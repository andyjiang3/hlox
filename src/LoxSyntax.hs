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
  deriving (Eq, Show)

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

type Name = String -- name of a variable

-- produce an effect
data Statement
  = Assign LValue Expression -- var x = e
  | If Expression Block Block -- if (e) { s1 } else { s2 }
  | While Expression Block -- while (e) { s }
  | For Name Expression Block -- for (var x = e; e; e) { s }, TODO: make sure this is correct
  | FunctionCallStatement Expression [Expression] -- f(e1, ..., en), TODO: Name instead of expresssion?
  | FunctionDef Name [Name] Block -- fun f(x1, ..., xn) { s }
  | Return Expression -- return e
  | Print Expression -- print e
  | Empty -- ';'
  deriving (Eq, Show)

-- produce a value, can be assigned or used as an operand
data Expression
  = Var Name -- global variables x
  | Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  | Grouping Expression -- (e)
  | FunctionCall Expression [Expression] -- f(e1, ..., en)
  deriving (Eq, Show)

data LValue
  = LName Name -- x, global variable
  | LArrayIndex LValue Expression -- a[i], TODO: support
  deriving (Eq, Show)

data Value -- literals
  = NilVal -- nil
  | IntVal Int -- 1
  | BoolVal Bool -- false, true
  | StringVal String -- "abd"
  deriving
    ( Eq,
      Show,
      Ord
    )

-- | FunctionVal [Name] Block -- function (x1, ..., xn) s end, todo: constraint name more?
data Uop
  = Neg -- `-` :: Int -> Int
  | Not -- `!` :: a -> Bool
  deriving (Eq, Show, Enum, Bounded)

data Bop
  = Plus -- `+`  :: Int -> Int -> Int
  | Minus -- `-`  :: Int -> Int -> Int
  | Times -- `*`  :: Int -> Int -> Int
  | Divide -- `/` :: Int -> Int -> Int   -- floor division
  | Modulo -- `%`  :: Int -> Int -> Int
  | Eq -- `==` :: a -> a -> Bool
  | Ne -- `!=` :: a -> a -> Bool
  | Gt -- `>`  :: a -> a -> Bool
  | Ge -- `>=` :: a -> a -> Bool
  | Lt -- `<`  :: a -> a -> Bool
  | Le -- `<=` :: a -> a -> Bool
  deriving (Eq, Show, Enum, Bounded)

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
