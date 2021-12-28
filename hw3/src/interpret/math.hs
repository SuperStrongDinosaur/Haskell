{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Math where

import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Control.Monad.Catch
import Data.Typeable

type Name = String
type Dictionary = M.Map Name Int

data Expr = Lit Int 
          | Var Name
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let Name Expr Expr
    deriving (Eq, Show)

data EvalError = DivizionByZero
               | NoVarBindingFound Name
    deriving (Eq, Show, Typeable)

instance Exception EvalError

exprEvaluation :: (MonadReader Dictionary m, MonadThrow m) => Expr -> m Int
exprEvaluation (Lit i) = return i
exprEvaluation (Var v) = do
    val <- asks (M.lookup v)
    case val of
      Just i -> return i
      Nothing -> throwM $ NoVarBindingFound v
exprEvaluation (Add l r) = liftM2 (+) (exprEvaluation l) (exprEvaluation r)
exprEvaluation (Sub l r) = liftM2 (-) (exprEvaluation l) (exprEvaluation r)
exprEvaluation (Mul l r) = liftM2 (*) (exprEvaluation l) (exprEvaluation r)
exprEvaluation (Div l r) = do
    r' <- exprEvaluation r
    if r' == 0 then
      throwM DivizionByZero
    else do
      l' <- exprEvaluation l
      return $ div l' r'
exprEvaluation (Let v d e) = do
    d' <- exprEvaluation d
    local (M.insert v d') (exprEvaluation e)

doEval :: (MonadThrow m) => Expr -> Dictionary -> m Int
doEval expr = runReaderT (exprEvaluation expr)

         