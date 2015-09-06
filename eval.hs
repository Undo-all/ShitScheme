{-# LANGUAGE TupleSections #-}

module Eval where

import Scheme
import qualified Data.Map as M (lookup, insert, fromList, union)
import Control.Monad.Except
import Parse
import Text.ParserCombinators.Parsec (parse)
import Data.IORef 

evalStr :: Env -> String -> String -> IO (Either String Value)
evalStr envRef name xs = 
    case parse parseExprs name xs of
      Left err  -> return (Left (show err))
      Right xs  -> either (Left . show) Right <$> runExceptT (evalExprs envRef xs)

evalExprs :: Env -> [Expr] -> Evaluator
evalExprs envRef [x]    = eval envRef x
evalExprs envRefRef (x:xs) = do eval envRefRef x
                                evalExprs envRefRef xs

eval :: Env -> Expr -> Evaluator
eval envRefRef (Atom (Symbol s)) = do
    envRef <- lift $ readIORef envRefRef
    case M.lookup s envRef of
      Just v -> do val <- lift $ readIORef v
                   return val
      _      -> throwError $ NotFound s
eval envRef (Atom v) = return v

eval envRef (List []) = throwError $ EmptyList
eval envRef (List (x:xs)) = do
    fn <- eval envRef x
    case fn of
      Form name numArgs f -> evalForm envRef name numArgs f xs
      _ -> mapM (eval envRef) xs >>= apply envRef fn

evalForm :: Env -> Name -> NumArgs -> (Env -> [Expr] -> Evaluator) -> [Expr] -> Evaluator
evalForm envRef name numArgs f args
    | rightNumArgs (length args) numArgs = f envRef args
    | otherwise                          = throwError $ WrongNumArgs name (length args) numArgs

apply :: Env -> Value -> [Value] -> ExceptT Error IO Value
apply envRef (Prim name numArgs f) args
    | rightNumArgs (length args) numArgs = f envRef args
    | otherwise                          = throwError $ WrongNumArgs name (length args) numArgs
apply envRef (Func name argNames body closure) args
    | length args /= length argNames = throwError $ WrongNumArgs name (length args) (makeNumArgs (length argNames))
    | otherwise = (liftIO $ bindVars closure $ zip argNames args) >>= evalBody
    where makeNumArgs n = (n, Just n)
          evalBody env = liftM last $ mapM (eval env) body
          bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
            where extendEnv bindings env  = liftM (M.union env . M.fromList) (mapM addBinding bindings)
                  addBinding (var, value) = do ref <- newIORef value
                                               return (var, ref)

rightNumArgs :: Int -> NumArgs -> Bool
rightNumArgs n (x, Nothing) = n >= x
rightNumArgs n (x, Just y)  = n >= x && n <= y

