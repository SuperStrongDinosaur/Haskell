{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Program where 

import Math         

import Control.Monad       
import Control.Monad.Catch 
import Control.Monad.State 
import qualified Data.Map.Strict as M
import Data.Typeable       

instance Exception StatementError

data Statement = NewVarStat String Expr 
               | UpdVarStat String Expr 
               | PrintStat Expr 
               | ReadStat String 
               | ForStat Expr Expr [Statement] 
               | BreakStat
  deriving (Eq, Show)

data StatementError = VarRedefError String
                    | UndefinedVarError String
                    | EvaluationError Statement EvalError
  deriving (Eq, Typeable)

instance Show StatementError where
  show (VarRedefError s) = "Redefinition of " ++ s
  show (UndefinedVarError s) = "Undefined variable " ++ s
  show (EvaluationError stmt e) = "Error \"" ++ show e ++ "\"in statement " ++ show stmt

def :: (MonadState Dictionary m, MonadCatch m) => String -> Int -> m ()
def v val = do
  m <- get
  case M.lookup v m of
    Nothing -> put $ M.insert v val m
    Just _ -> throwM (VarRedefError v)

update :: (MonadState Dictionary m, MonadCatch m) => String -> Int -> m ()
update v val = do
  m <- get
  case M.lookup v m of
    Nothing -> throwM (UndefinedVarError v)
    Just _ -> put $ M.insert v val m

overwrite :: (MonadState Dictionary m, MonadCatch m) => String -> Int -> m ()
overwrite v val = modify $ M.insert v val

compute :: (MonadState Dictionary m, MonadCatch m, MonadIO m) => Statement -> m ()
compute x = do
  vars <- get
  c <- case x of
    BreakStat -> return 0
    ReadStat _ -> read <$> liftIO getLine
    _ -> evalExpr x (takeExpr x) vars where 
      takeExpr (NewVarStat _ y) = y
      takeExpr (PrintStat y) = y
      takeExpr (ForStat y _ _) = y
      takeExpr (UpdVarStat _ y) = y
  case x of
    NewVarStat v _ -> def v c
    UpdVarStat v _ -> update v c
    PrintStat _ -> liftIO $ print c
    ReadStat v -> overwrite v c
    ForStat _ bound sts -> do
      to <- evalExpr x bound vars
      replicateM_ (to - c) (exec sts) 
    BreakStat -> return ()
  where evalExpr stmt expr vz = catch (doEval expr vz) (throwM . EvaluationError stmt)

exec :: (MonadState Dictionary m, MonadCatch m, MonadIO m) => [Statement] -> m Dictionary
exec [] = get
exec (x : xs) = do 
  compute x
  exec xs

newtype StatementCtx a = StatementCtx { runStatement :: StateT Dictionary IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Dictionary, MonadThrow, MonadCatch)

executeProgram :: StatementCtx a -> IO a
executeProgram x = fst <$> runStateT (runStatement x) M.empty

--tmp :: [Statement] -> ()
--tmp x = void $ fst <$> runStateT (runStatement $ exec $ x) M.empty)

interpret :: [Statement] -> IO ()
interpret = void . executeProgram . exec






