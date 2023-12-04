module LoxStepper where

import Control.Applicative
import Data.Char qualified as Char
import LoxSyntax
import LoxParser
import ParserLib (Parser)
import ParserLib qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import State (State)
import State qualified as S
import Data.List qualified as List
import Control.Monad (guard)
import Data.Map (Map, (!?))
import Text.Read (readMaybe)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP


type Environment = Map Name Table

type Table = Map Value Value

data Store = St {environment :: Environment, parent :: Maybe Store} deriving (Eq, Show)


instance PP Store where
  pp (St e _) = pp e

globalTableName  :: Name
globalTableName = "_G"

emptyEnv :: Map Name (Map k a)
emptyEnv = Map.singleton globalTableName Map.empty

initialStore :: Store
initialStore = St { environment = emptyEnv, parent = Nothing}




type Reference = (Name, Value)

index :: Reference -> State Store Value
index (t, n) = do
  store <- S.get
  let valMaybe = let env = environment store in env !? t >>= (!? n)
  let u :: Value = case parent store of 
        Nothing -> NilVal
        Just st -> let b = index (t,n) in S.evalState b st
  return $ fromMaybe u valMaybe


updateOnce :: Reference -> Value -> State Store Bool 
updateOnce (_, NilVal) _ = return True
updateOnce (table, name) val = do
  store <- S.get
  let newStore :: Maybe Store =
        do
          oldTable <- environment store !? table
          guard (isJust $ Map.lookup name oldTable)
          let newTable = Map.insert name val oldTable
          return $ store {environment = Map.insert table newTable (environment store)}
  S.put $ fromMaybe store newStore
  return $ isJust newStore


update :: Reference -> Value -> State Store ()
update (_, NilVal) _ = return ()
update ref@(t, n) val = do
  store <- S.get
  v <- updateOnce ref val 
  if  v then return () else
    case parent store of 
            Nothing -> return ()
            Just p -> let newParent :: Store = S.execState (updateOnce ref val) p in
                let st2 = St {environment = environment store, parent = Just newParent} in
                    S.put st2


allocate :: Reference -> Value -> State Store ()
allocate (_, NilVal) _ = return ()
allocate (table, name) val = do
  store <- S.get
  let newStore :: Maybe Store =
        do
          oldTable <- environment store !? table
          guard ( isNothing $ Map.lookup name oldTable)
          let newTable = Map.insert name val oldTable
          return $ store { environment = Map.insert table newTable (environment store)}
  S.put $ fromMaybe store newStore



resolve :: LValue -> Reference
resolve (LName n) = (globalTableName, StringVal n)
resolve _ = undefined


functionPrologue :: Expression -> [Name] -> [Expression] -> State Store ()
functionPrologue exp names args = do
    st <- S.get
    let st2 = St{environment =  emptyEnv, parent = Just st } in
        let action1 = S.put st2 in 
            let action2 = let pairs = zip names args in mapM_ (\(name, arg) -> let ref = resolve (LName name) in do 
                        val <- evalE arg
                        allocate ref val) pairs in sequence_ [action1, action2]




functionEpilogue :: State Store ()
functionEpilogue = do
    st <- S.get
    S.put $ fromMaybe st (parent st)
    

evalE :: Expression -> State Store Value
evalE (Var name) = index (globalTableName, StringVal name)
evalE (Val v) = return v
evalE (Op2 e1 o e2) = evalOp2 o <$> evalE e1 <*> evalE e2
evalE (Op1 _o _e1) = evalE _e1 >>= evalOp1 _o
evalE (FunctionCall e es) = do 
    fun <- evalE e
    case fun of 
        FunctionVal names blk -> do functionPrologue e names es; 
                                    val <- eval blk
                                    return $ fromMaybe NilVal val
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

evalS :: Statement -> State Store (Maybe Value)
evalS (Assign lv e) = do 
    val <- evalE e
    let ref = resolve lv
    update ref val
    return Nothing
    
evalS (VarDecl n e ) = do
    val <- evalE e
    let ref = (globalTableName, StringVal n)
    allocate ref val
    return Nothing
evalS (Return e) = do 
    val <- evalE e
    functionEpilogue
    return (Just val)
evalS (FunctionDef name names blk) = do
  let ref = (globalTableName,  StringVal name)
  let fun = FunctionVal names blk
  allocate ref fun
  return Nothing
evalS (FunctionCallStatement name args) = do
  fun <- evalE name
  case fun of 
        FunctionVal names blk -> do functionPrologue name names args;
                                      eval blk
        _ -> return Nothing 
evalS _ = undefined


-- evaluate a block to completion
-- add support for early exit if there is a return
eval :: Block -> State Store (Maybe Value)
eval (Block ss) = go ss
  where
    go [] = do return Nothing
    go [s] = evalS s
    go (s : ss) = do
      evalS s
      go ss

-- execute a block on a store
exec :: Block -> Store -> Store
exec = S.execState . eval


-- step over a block on statement at a time
-- add support for early exit if there is a return
step :: Block -> State Store Block
step b@(Block []) = return b
step (Block (If e (Block b1) (Block b2) : ss)) = do
  v <- evalE e
  return $ Block $ if toBool v then b1 ++ ss else b2 ++ ss
step b@(Block (While e wb@(Block []) : ss)) = do
  v <- evalE e
  if toBool v
    then return b -- infinite while loop because loop is empty
    else return $ Block ss
step (Block w@(While e wb : ss)) = do
  v <- evalE e
  if toBool v
    then do
      case wb of
        Block bs -> return $ Block $ bs ++ w
    else -- return (wb : w)
      return $ Block ss
step (Block (s : ss)) = do
  evalS s
  return $ Block ss



-- step opver a block for a number of statements
boundedStep :: Int -> Block -> State Store Block
boundedStep i b | i > 0 = do
  b' <- step b
  boundedStep (i - 1) b'
boundedStep _ b = return b

-- exectute bounder step over a store
steps :: Int -> Block -> Store -> (Block, Store)
steps n block = S.runState (boundedStep n block)


prop_step_total :: Block -> Store -> Bool
prop_step_total b s = case S.runState (step b) s of
  (b', s') -> True

-- test to see if a block has finished
final :: Block -> Bool
final (Block []) = True
final _ = False

-- | Evaluate this block to completion
execStep :: Block -> Store -> Store
execStep b | final b = id
execStep b = uncurry execStep . steps 1 b


prop_stepExec :: Block -> QC.Property
prop_stepExec b =
  not (final b) QC.==> final b1 QC.==> m1 == m2
  where
    (b1, m1) = S.runState (boundedStep 100 b) initialStore
    m2 = exec b initialStore





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
            (Left _) -> do
              putStrLn "invalid file"
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
           in go ss {block = mempty, store = s', history = Just ss}
        -- next statement (could be multiple)
        Just (":n", strs) -> do
          let numSteps :: Int
              numSteps = case readMaybe (concat strs) of
                Just x -> x
                Nothing -> 1

          let res = exec numSteps ss in go res
          where
            exec n stepper
              | n <= 0 = stepper
              | case block stepper of Block xs -> null xs = stepper
              | otherwise = case history stepper of
                  Just prevStepper -> exec (n - 1) $ stepper {block = newBlock, store = newStore, history = Just stepper {history = Just prevStepper}}
                  Nothing -> exec (n - 1) $ stepper {block = newBlock, store = newStore, history = Just stepper}
              where
                (newBlock, newStore) = steps 1 (block stepper) (store stepper)

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
    prompt Stepper {block = Block []} = return ()
    prompt Stepper {block = Block (s : _)} =
      putStr "--> " >> putStrLn (pretty s)
