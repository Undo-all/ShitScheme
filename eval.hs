{-# LANGUAGE TupleSections #-}

module Eval where

import Scheme
import qualified Data.Map as M (lookup, insert)
import Control.Monad.Except

evalExprs :: Env -> [Expr] -> Evaluator
evalExprs env [x]    = eval env x
evalExprs env (x:xs) = (snd <$> eval env x) >>= flip evalExprs xs

eval :: Env -> Expr -> Evaluator
eval env (Atom (Symbol s)) = 
    case M.lookup s env of
      Just v -> return (v, env)
      _      -> throwError $ NotFound s
eval env (Atom v) = return (v, env)

eval env (List []) = throwError $ EmptyList
eval env (List (x:xs)) = do
    fn <- fst <$> eval env x
    case fn of
      Form name numArgs f -> evalForm env name numArgs f xs
      _ -> (,env) <$> (mapM (fmap fst . eval env) xs >>= apply env fn)

evalForm :: Env -> Name -> NumArgs -> (Env -> [Expr] -> Evaluator) -> [Expr] -> Evaluator
evalForm env name numArgs f args
    | rightNumArgs (length args) numArgs = f env args
    | otherwise                          = throwError $ WrongNumArgs name (length args) numArgs

apply :: Env -> Value -> [Value] -> ExceptT Error IO Value
apply env (Prim name numArgs f) args
    | rightNumArgs (length args) numArgs = f env args
    | otherwise                          = throwError $ WrongNumArgs name (length args) numArgs
apply env (Func name argNames body) args
    | length args /= length argNames = throwError $ WrongNumArgs name (length args) (makeNumArgs (length argNames))
    | otherwise = fst <$> evalExprs newEnv body
    where makeNumArgs n = (n, Just n)
          newEnv = foldl (flip (uncurry M.insert)) env (zip argNames args)

rightNumArgs :: Int -> NumArgs -> Bool
rightNumArgs n (x, Nothing) = n >= x
rightNumArgs n (x, Just y)  = n >= x && n <= y

