module LoxArbitrary where

import Control.Applicative
import Control.Monad (guard)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import LoxStepper
import LoxSyntax
import ParserLib (Parser)
import ParserLib qualified as P
import State (State)
import State qualified as S
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP
import Text.Read (readMaybe)
-- hi
quickCheckN :: (QC.Testable prop) => Int -> prop -> IO ()
quickCheckN n = QC.quickCheckWith $ QC.stdArgs {QC.maxSuccess = n, QC.maxSize = 100}

genId :: Gen Id
genId = arbitrary

-- Generator for Maybe Id
genMaybeId :: Gen (Maybe Id)
genMaybeId =
  QC.frequency
    [ (1, pure Nothing),
      (2, Just <$> genId)
    ]

-- Generator for Table
genTable :: Gen Table
genTable = Map.fromList <$> QC.listOf ((,) <$> arbitrary <*> arbitrary)

-- Generator for Environment
genEnvironment :: Gen Environment
genEnvironment = Env <$> genMemory <*> genMaybeId
  where
    genMemory :: Gen (Map Name Table)
    genMemory = Map.fromList <$> QC.listOf ((,) <$> arbitrary <*> genTable)

-- Generator for Environments
genEnvironments :: Gen Environments
genEnvironments = Map.fromList <$> QC.listOf ((,) <$> genId <*> genEnvironment)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe ga = QC.chooseInteger (0, 10) >>= \p -> if p < 5 then ga >>= \x -> return (Just x) else return Nothing

genStack :: Int -> Gen Stack
genStack 0 = Stk <$> arbitrary <*> pure Nothing
genStack n = Stk <$> arbitrary <*> genMaybe stk
  where
    stk = genStack (n - 1)

genStore :: Int -> Gen Store
genStore n = St <$> arbitrary <*> genEnvironments <*> genStack n

instance Arbitrary Store where
  arbitrary = QC.sized genStore
  shrink _ = []

-- Syntax generators --

-- Generate a small set of names for generated tests. These names are guaranteed to not include reserved words
genName :: Gen Name
genName = QC.elements ["_", "_G", "x", "X", "y", "x0", "X0", "xy", "XY", "_x"]

-- Generate a string literal, being careful about the characters that it may contain
genStringLit :: Gen String
genStringLit = escape <$> QC.listOf (QC.elements stringLitChars)
  where
    -- escape special characters appearing in the string,
    escape :: String -> String
    escape = foldr Char.showLitChar ""
    -- generate strings containing printable characters or spaces, but not including '\"'
    stringLitChars :: [Char]
    stringLitChars = filter (\c -> c /= '\"' && (Char.isSpace c || Char.isPrint c)) ['\NUL' .. '~']

genString :: Gen String
genString = QC.vectorOf 5 $ QC.elements "abcdefg"

-- | access the first statement in a block, if one exists
first :: Block -> [Statement]
first (Block []) = []
first (Block (x : _)) = [x]

genLValue :: Int -> Gen LValue
genLValue 0 = LName <$> genString
genLValue n =
  QC.frequency
    [ (1, LName <$> genString),
      (1, LArrayIndex <$> genLValue n' <*> genExp n')
    ]
  where
    n' = n `div` 2

genExp :: Int -> Gen Expression
genExp 0 = QC.oneof [Var <$> genString, Val <$> arbitrary]
genExp n =
  QC.frequency
    [ (1, Var <$> genString),
      (1, Val <$> arbitrary),
      (1, ArrayIndex <$> genExp n' <*> genExp n'),
      (1, ArrayCons <$> QC.vectorOf 3 (genExp n')),
      (n, Op1 <$> arbitrary <*> genExp n'),
      (n, Op2 <$> genExp n' <*> arbitrary <*> genExp n'),
      (0, FunctionCall <$> genExp n' <*> QC.vectorOf 3 (genExp n'))
    ]
  where
    n' = n `div` 2

genStatement :: Int -> Gen Statement
genStatement n | n <= 1 = QC.oneof [Assign <$> genLValue 0 <*> genExp 0, return Empty]
genStatement n =
  let name = genName
   in QC.frequency
        [ (1, Assign <$> genLValue n' <*> genExp n'),
          (1, VarDecl <$> genName <*> genExp n'),
          (1, return Empty),
          (n, If <$> genExp n' <*> genBlock n' <*> genBlock n'),
          -- generate loops half as frequently as if statements
          (n', For <$> (VarDecl <$> name <*> genExp n') <*> genExp n' <*> (Assign <$> (LName <$> name) <*> genExp n') <*> genBlock n'),
          (n', While <$> genExp n' <*> genBlock n'),
          (n', FunctionCallStatement <$> (Var <$> genName) <*> QC.vectorOf 3 (genExp n')),
          (n', FunctionDef <$> genName <*> QC.vectorOf 3 genName <*> genBlock n'), -- TODO: make sure it always have return at the end
          (n', Return <$> genExp n')
        ]
  where
    n' = n `div` 2

genBlock :: Int -> Gen Block
genBlock n = Block <$> genStmts n
  where
    genStmts 0 = pure []
    genStmts n =
      QC.frequency
        [ (1, return []),
          (n, (:) <$> genStatement n' <*> genStmts n')
        ]
      where
        n' = n `div` 2

instance Arbitrary LValue where
  arbitrary = QC.sized genLValue
  shrink (LName n) = []
  shrink (LArrayIndex e n) = [LArrayIndex e n' | n' <- shrink n]

instance Arbitrary Uop where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Bop where
  arbitrary = QC.arbitraryBoundedEnum

shrinkStringLit :: String -> [String]
shrinkStringLit s = filter (/= '\"') <$> shrink s

instance Arbitrary Value where
  arbitrary =
    QC.oneof
      [ IntVal <$> arbitrary,
        BoolVal <$> arbitrary,
        pure NilVal,
        StringVal <$> genStringLit,
        FunctionValIncomplete <$> QC.vectorOf 3 genName <*> genBlock 3
        -- Don't generate ArrayVal since it isnt used in the interpreter
      ]

  shrink (IntVal n) = IntVal <$> shrink n
  shrink (BoolVal b) = BoolVal <$> shrink b
  shrink NilVal = []
  shrink (StringVal s) = StringVal <$> shrinkStringLit s
  shrink (FunctionValIncomplete ns b) = [FunctionValIncomplete ns b' | b' <- shrink b]
  shrink _ = undefined

instance Arbitrary Expression where
  arbitrary = QC.sized genExp

  shrink (Op1 o e) = e : [Op1 o e' | e' <- shrink e]
  shrink (Op2 e1 o e2) =
    [Op2 e1' o e2 | e1' <- shrink e1]
      ++ [Op2 e1 o e2' | e2' <- shrink e2]
      ++ [e1, e2]
  shrink (ArrayIndex e1 e2) = [ArrayIndex e1' e2 | e1' <- shrink e1] ++ [ArrayIndex e1 e2' | e2' <- shrink e2]
  shrink (ArrayCons es) = [ArrayCons es' | es' <- shrink es]
  shrink (FunctionCall e es) = [FunctionCall e' es | e' <- shrink e] ++ [FunctionCall e es' | es' <- shrink es]
  shrink _ = []

instance Arbitrary Statement where
  arbitrary = QC.sized genStatement
  shrink (Assign v e) =
    [Assign v' e | v' <- shrink v]
      ++ [Assign v e' | e' <- shrink e]
  shrink (If e b1 b2) =
    first b1
      ++ first b2
      ++ [If e' b1 b2 | e' <- shrink e]
      ++ [If e b1' b2 | b1' <- shrink b1]
      ++ [If e b1 b2' | b2' <- shrink b2]
  shrink (While e b) =
    first b
      ++ [While e' b | e' <- shrink e]
      ++ [While e b' | b' <- shrink b]
  shrink Empty = []
  shrink (VarDecl n e) = [VarDecl n e' | e' <- shrink e]
  shrink (For s1 e s2 b) =
    first b
      ++ [For s1' e s2 b | s1' <- shrink s1]
      ++ [For s1 e' s2 b | e' <- shrink e]
      ++ [For s1 e s2' b | s2' <- shrink s2]
      ++ [For s1 e s2 b' | b' <- shrink b]
  shrink (FunctionCallStatement e es) = [FunctionCallStatement e' es | e' <- shrink e] ++ [FunctionCallStatement e es' | es' <- shrink es]
  shrink (FunctionDef n ns b) = first b ++ [FunctionDef n ns b' | b' <- shrink b]
  shrink (Return e) = [Return e' | e' <- shrink e]
  shrink _ = []

instance Arbitrary Block where
  arbitrary = QC.sized genBlock
  shrink (Block ss) = [Block ss' | ss' <- shrink ss]