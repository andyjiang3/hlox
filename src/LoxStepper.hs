module LoxStepper where

import Control.Applicative
import Control.Monad (guard)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import LoxParser
import LoxSyntax
import ParserLib (Parser)
import ParserLib qualified as P
import State (State)
import State qualified as S
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP
import Text.Read (readMaybe)

type Table = Map Value Value

type EitherBlock = Either String Block 

type EitherStore = Either String Store

-- local variables a scope is working with, helps find where the correct parent environment is for closures
data Environment = Env {memory :: Map Name Table, parent :: Maybe Id} deriving (Eq, Show)

-- map from environment id to an actual environment
type Environments = Map Id Environment

-- Stack of environments id, helps go back when scope ended
data Stack = Stk {curr :: Id, par :: Maybe Stack} deriving (Eq, Show)

-- Store holds current environment, stack the map of Environemnts
data Store = St {environment :: Id, environments :: Environments, stack :: Stack} deriving (Eq, Show)


instance PP Store where
  pp :: Store -> Doc
  pp (St e es _) = case Map.lookup e es of
    Nothing -> PP.text "empty"
    Just env -> pp (memory env)

globalTableName :: Name
globalTableName = "_G"

emptyEnv :: Environment
emptyEnv = Env {memory = Map.singleton globalTableName Map.empty, parent = Nothing}

initialStore :: Store
initialStore = St {environment = 0, environments = Map.fromList [(0, emptyEnv)], stack = Stk {curr = 0, par = Nothing}}

type Reference = (Name, Value)

getEnvironment :: Id -> State Store (Maybe Environment)
getEnvironment id = do Map.lookup id . environments <$> S.get

indexRecursive :: Reference -> Id -> State Store Value
indexRecursive (t, n) id = do
  store <- S.get
  maybeEnv <- getEnvironment id
  let env = fromMaybe emptyEnv maybeEnv
  let valMaybe = memory env !? t >>= (!? n)
  case valMaybe of
    Just val -> return val
    Nothing -> case parent env of
      Just p -> indexRecursive (t, n) p
      Nothing -> return NilVal

index :: Reference -> State Store Value
index ref@(t, n) = do
  store <- S.get
  indexRecursive ref (environment store)

updateRecursive :: Reference -> Id -> Value -> State Store Value
updateRecursive ref@(table, name) id val = do
  store <- S.get
  let newStore :: Maybe Store =
        do
          env <- environments store !? id
          oldTable <- memory env !? table
          guard (isJust $ Map.lookup name oldTable)
          let newTable = Map.insert name val oldTable
          let newMemory = Map.insert table newTable (memory env)
          return $ store {environments = Map.insert id (env {memory = newMemory}) (environments store)}
  case newStore of
    Just new -> do 
      S.put new
      return NilVal
    Nothing -> case environments store !? id of
      Nothing -> return (ErrorVal ("Variable " ++ pretty name ++  " Does not exists" ))
      Just env -> case parent env of
        Nothing -> return (ErrorVal ("Variable " ++ pretty name ++  " Does not exists" ))
        Just p -> updateRecursive ref p val

-- updates only existing variable
update :: Reference -> Value -> State Store Value
update (_, NilVal) _ = return (ErrorVal "Cannot update value to be nil")
update ref@(t, n) val = do
  store <- S.get
  updateRecursive ref (environment store) val

-- defines the variable
allocate :: Reference -> Value -> State Store Value
allocate (_, NilVal) _ = return (ErrorVal "Cannot create nil values")
allocate (table, name) val = do
  store <- S.get
  let newStore :: Maybe Store =
        do
          env <- environments store !? environment store
          oldTable <- memory env !? table
          guard (isNothing $ Map.lookup name oldTable)
          let newTable = Map.insert name val oldTable
          let newMemory = Map.insert table newTable (memory env)
          return $ store {environments = Map.insert (environment store) (env {memory = newMemory}) (environments store)}
  case newStore of 
    Nothing -> return (ErrorVal ("Multiple definitons. Variable " ++ pretty name ++  "Already exists" ))
    Just ss -> do 
      S.put ss
      return NilVal

resolve :: LValue -> Reference
resolve (LName n) = (globalTableName, StringVal n)
resolve _ = undefined

functionPrologue :: Expression -> [Name] -> [Expression] -> Id -> State Store ()
functionPrologue exp names args id = do
  st <- S.get
  enterScope f
  let pairs = zip names args
   in mapM_
        ( \(name, arg) ->
            let ref = resolve (LName name)
             in do
                  val <- evalE arg
                  allocate ref val
        )
        pairs
  where
    f = functionEnterScope id

enterScope :: (Store -> Store) -> State Store ()
enterScope f = do
  st <- S.get
  S.put $ f st


defaultEnterScope :: State Store ()
defaultEnterScope = do
  enterScope f
  where
    f st = st {environment = n, environments = Map.insert n newEnv (environments st), stack = newStack}
      where
        newEnv = emptyEnv {parent = Just $ environment st}
        n = length (Map.keys (environments st))
        newStack = Stk {curr = n, par = Just $ stack st}

-- modify such that the parent of the new Env should be fetched from the function value
functionEnterScope :: Id -> Store -> Store
functionEnterScope id st = st {environment = n, environments = Map.insert n newEnv (environments st), stack = newStack}
  where
    newEnv = emptyEnv {parent = Just id}
    n = length (Map.keys (environments st))
    newStack = Stk {curr = n, par = Just $ stack st}

exitScope :: State Store ()
exitScope = do
  store <- S.get
  let newStore :: Maybe Store =
        do
          p <- par (stack store)
          return store {environment = curr p, stack = p}
  S.put $ fromMaybe store newStore

functionEpilogue :: State Store ()
functionEpilogue = exitScope

evalE :: Expression -> State Store Value
evalE (Var name) = index (globalTableName, StringVal name)
evalE (Val (FunctionValIncomplete names blk)) = do FunctionVal names blk . environment <$> S.get
evalE (Val v) = return v
evalE (Op2 e1 o e2) = evalOp2 o <$> evalE e1 <*> evalE e2
evalE (Op1 _o _e1) = evalE _e1 >>= evalOp1 _o
evalE (FunctionCall e es) = do
  fun <- evalE e
  case fun of
    FunctionVal names blk id -> do
      functionPrologue e names es id
      eval blk
    _ -> return NilVal
evalE _ = undefined

toBool :: Value -> Bool
toBool (BoolVal False) = False
toBool NilVal = False
toBool _ = True

evalOp1 :: Uop -> Value -> State Store Value
evalOp1 Neg (IntVal i) = return $ IntVal $ -i
evalOp1 Not v = return $ BoolVal $ not $ toBool v
evalOp1 _ _ = return NilVal

evalOp2 :: Bop -> Value -> Value -> Value
evalOp2 Plus (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
evalOp2 Minus (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
evalOp2 Times (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
evalOp2 Divide (IntVal _) (IntVal 0) = NilVal
evalOp2 Divide (IntVal i1) (IntVal i2) = IntVal (i1 `div` i2)
evalOp2 Modulo (IntVal _) (IntVal 0) = NilVal
evalOp2 Modulo (IntVal i1) (IntVal i2) = IntVal (i1 `mod` i2)
evalOp2 Eq v1 v2 = BoolVal $ v1 == v2
evalOp2 Gt v1 v2 = BoolVal $ v1 > v2
evalOp2 Ge v1 v2 = BoolVal $ v1 >= v2
evalOp2 Lt v1 v2 = BoolVal $ v1 < v2
evalOp2 Le v1 v2 = BoolVal $ v1 <= v2
evalOp2 _ _ _ = NilVal

evaluate :: Expression -> Store -> Value
evaluate e = S.evalState (evalE e)

-- Everything here is not sure

evalS :: Statement -> State Store Value
evalS (Assign lv e) = do
  val <- evalE e
  let ref = resolve lv
  update ref val
evalS (VarDecl n e) = do
  val <- evalE e
  let ref = (globalTableName, StringVal n)
  allocate ref val
evalS (Return e) = do
  val <- evalE e
  functionEpilogue
  return  val
evalS (FunctionDef name names blk) = do
  st <- S.get
  let ref = (globalTableName, StringVal name)
  let fun = FunctionVal names blk (environment st)
  allocate ref fun
evalS (FunctionCallStatement name args) = do
  fun <- evalE name
  case fun of
    FunctionVal names blk id -> do
      functionPrologue name names args id
      eval blk
    _ -> return $ ErrorVal (pretty fun ++ "Is not a Function")
evalS EndStatement = do
  exitScope
  return NilVal
evalS _ = return NilVal

-- evaluate a block to completion
-- add support for early exit if there is a return
eval :: Block -> State Store Value
eval (Block ss) = go ss
  where
    go [] = do return NilVal
    go [s] = evalS s
    go (s : ss) = do
      evalS s
      go ss

-- execute a block on a store
exec :: Block -> Store -> Store
exec = S.execState . eval

-- step over a block on statement at a time
-- add support for early exit if there is a return
step :: Block -> State Store EitherBlock
step b@(Block []) = return $ Right b
step (Block (If e (Block b1) (Block b2) : ss)) = do
  v <- evalE e
  case v of
    ErrorVal s -> return $ Left s
    _ -> do 
      defaultEnterScope
      return $ Right $ Block $ if toBool v then b1 ++ [EndStatement] ++ ss else b2 ++ [EndStatement] ++ ss
step b@(Block (While e wb@(Block []) : ss)) = do
  v <- evalE e
  case v of 
    ErrorVal s -> return $ Left s
    _ -> do
          if toBool v
            then return $ Right b -- infinite while loop because loop is empty
            else return $ Right $ Block ss
step (Block w@(While e wb : ss)) = do
  v <- evalE e
  case v of 
    ErrorVal s -> return $ Left s
    _ -> do
      if toBool v
        then do
          case wb of
            Block bs -> return $ Right $ Block $ bs ++ w
        else -- return (wb : w)
          return $ Right $ Block ss
step (Block (Empty : ss)) = step $ Block ss
step (Block (EndStatement : ss)) = do evalS EndStatement; step $ Block ss
step (Block (s : ss)) = do
  v <- evalS s
  case v of 
    ErrorVal s -> return $ Left s 
    _ -> do return $ Right $ Block ss

-- step opver a block for a number of statements
boundedStep :: Int -> Block -> State Store EitherBlock
boundedStep i b | i > 0 = do
  b' <- step b
  case b' of
    Left s -> return $ Left s
    Right b'' ->  boundedStep (i - 1) b''
boundedStep _ b = return $ Right b

-- exectute bounder step over a store
steps :: Int -> Block -> Store -> (EitherBlock, Store)
steps n block = S.runState (boundedStep n block)

-- test to see if a block has finished
final :: Block -> Bool
final (Block []) = True
final _ = False

-- | Evaluate this block to completion
execStep :: Block -> Store -> EitherStore
execStep b s | final b = Right s
execStep b s = case steps 1 b s of
  (Left err, _) -> Left err
  (Right newBlock, newStore) -> execStep newBlock newStore

data Stepper = Stepper
  { filename :: Maybe String,
    block :: Block,
    store :: Store,
    history :: Maybe Stepper
  }

initialStepper :: Stepper
initialStepper =
  Stepper
    { filename = Nothing,
      block = mempty,
      store = initialStore,
      history = Nothing
    }

stepper :: IO ()
stepper = go initialStepper
  where
    go :: Stepper -> IO ()
    go ss = do
      prompt ss
      putStr (fromMaybe "Lox" (filename ss) ++ "> ")
      str <- getLine
      case List.uncons (words str) of
        -- load a file for stepping
        Just (":l", [fn]) -> do
          result <- LoxParser.parseLoxFile fn
          case result of
            (Left s) -> do
              putStrLn "Error Loading or Parsing file"
              go ss
            (Right blck) -> do
              putStrLn ("Loaded " ++ fn ++ " ,initializing stepper")
              go ss {filename = Just fn, block = blck}
        -- go ss2
        -- dump the store
        Just (":d", _) -> do
          putStrLn (pretty (store ss))
          go ss
        -- quit the stepper
        Just (":q", _) -> return ()
        -- run current block to completion
        Just (":r", _) ->
          let s' = exec (block ss) (store ss)
           in go ss {block = mempty, store = initialStore, history = Just ss}
        -- next statement (could be multiple)
        Just (":n", strs) -> do
          let numSteps :: Int
              numSteps = case readMaybe (concat strs) of
                Just x -> x
                Nothing -> 1

          let res = exec numSteps ss in 
            case res of 
              Left err -> do putStrLn ("Runtime Error: " ++ err); go ss {block = mempty, store = initialStore, history = Just ss}
              Right newStepper -> go newStepper 
          where
            exec n stepper
              | n <= 0 = Right stepper
              | case block stepper of Block xs -> null xs = Right stepper
              | otherwise = case eitherBlock of
                  Left err -> Left err
                  Right newBlock ->
                    case history stepper of
                      Just prevStepper -> exec (n - 1) $ stepper {block = newBlock, store = newStore, history = Just stepper {history = Just prevStepper}}
                      Nothing -> exec (n - 1) $ stepper {block = newBlock, store = newStore, history = Just stepper}
              where
                (eitherBlock, newStore) = steps 1 (block stepper) (store stepper)
        Just (":p", strs) -> do
          let numSteps :: Int
              numSteps = case readMaybe (concat strs) of
                Just x -> x
                Nothing -> 1
          let res = reverse numSteps ss in go res
          where
            reverse n stepper
              | n <= 0 = stepper -- Nothing to reverse
              | otherwise = case history stepper of
                  Just prevStepper -> reverse (n - 1) prevStepper
                  Nothing -> stepper
        -- evaluate an expression in the current state
        _ -> case LoxParser.parseLoxExp str of
          Right exp -> do
            let v = evaluate exp (store ss)
            putStrLn (pretty v)
            go ss
          Left _s -> do
            putStrLn "?"
            go ss
    prompt :: Stepper -> IO ()
    prompt Stepper {block} = printFirst block

    printFirst :: Block -> IO ()
    printFirst (Block []) = return ()
    printFirst (Block (Empty : ss)) = printFirst (Block ss)
    printFirst (Block (EndStatement : ss)) = printFirst (Block ss)
    printFirst (Block (s : _)) = putStr "--> " >> putStrLn (pretty s)
