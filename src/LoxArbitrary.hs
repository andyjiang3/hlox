module LoxArbitrary where

import Control.Applicative
import Control.Monad (guard)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import LoxSyntax
import LoxStepper
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


quickCheckN :: (QC.Testable prop) => Int -> prop -> IO ()
quickCheckN n = QC.quickCheckWith $ QC.stdArgs {QC.maxSuccess = n, QC.maxSize = 100}

genName :: Gen Name
genName = QC.elements ["_", "_G", "x", "X", "y", "x0", "X0", "xy", "XY", "_x"]

genStringLit :: Gen String
genStringLit = escape <$> QC.listOf (QC.elements stringLitChars)
  where
    -- escape special characters appearing in the string,
    escape :: String -> String
    escape = foldr Char.showLitChar ""
    -- generate strings containing printable characters or spaces, but not including '\"'
    stringLitChars :: [Char]
    stringLitChars = filter (\c -> c /= '\"' && (Char.isSpace c || Char.isPrint c)) ['\NUL' .. '~']



genId :: Gen Id
genId = arbitrary

-- Generator for Maybe Id
genMaybeId :: Gen (Maybe Id)
genMaybeId = QC.frequency
  [ (1, pure Nothing)
  , (2, Just <$> genId)
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
genStack n = Stk <$> arbitrary <*> genMaybe stk where
  stk = genStack (n-1)


genStore:: Int -> Gen Store
genStore n = St <$> arbitrary <*> genEnvironments <*> genStack n

instance Arbitrary Store where 
  arbitrary = QC.sized genStore
  shrink = undefined


genLValue :: Int -> Gen LValue
genLValue 0 = LName <$> genName
genLValue n =
  QC.frequency
    [ (1, LName <$> genName),
      (0, LArrayIndex <$> genLValue n' <*> genExp n')
    ]
  where
    n' = n `div` 2

genExp :: Int -> Gen Expression
genExp 0 = QC.oneof [Var <$> genName, Val <$> arbitrary]
genExp n =
  QC.frequency
    [ (1, Var <$> genName),
      (1, Val <$> arbitrary),
      (n, Op1 <$> arbitrary <*> genExp n'),
      (n, Op2 <$> genExp n' <*> arbitrary <*> genExp n')
    ]
  where
    n' = n `div` 2


genStatement :: Int -> Gen Statement
genStatement n | n <= 1 = QC.oneof [Assign <$> genLValue 0 <*> genExp 0, return Empty]
genStatement n =
  QC.frequency
    [ (1, Assign <$> genLValue n' <*> genExp n'),
      (1, return Empty),
      (n, If <$> genExp n' <*> genBlock n' <*> genBlock n'),
      -- generate loops half as frequently as if statements
      (n', While <$> genExp n' <*> genBlock n')
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
  shrink _ = undefined

-- | access the first statement in a block, if one exists
first :: Block -> [Statement]
first (Block []) = []
first (Block (x : _)) = [x]

instance Arbitrary Block where
  arbitrary = QC.sized genBlock
  shrink (Block ss) = [Block ss' | ss' <- shrink ss]

instance Arbitrary Expression where
  arbitrary = QC.sized genExp

  shrink (Val v) = Val <$> shrink v
  shrink (Var v) = Var <$> shrink v
  shrink (Op1 o e) = e : [Op1 o e' | e' <- shrink e]
  shrink (Op2 e1 o e2) =
    [Op2 e1' o e2 | e1' <- shrink e1]
      ++ [Op2 e1 o e2' | e2' <- shrink e2]
      ++ [e1, e2]
  shrink _ = undefined

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
        StringVal <$> genStringLit
        -- note: do not generate table values
      ]

  shrink (IntVal n) = IntVal <$> shrink n
  shrink (BoolVal b) = BoolVal <$> shrink b
  shrink NilVal = []
  shrink (StringVal s) = StringVal <$> shrinkStringLit s
  shrink _ = undefined