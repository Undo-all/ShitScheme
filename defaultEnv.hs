{-# LANGUAGE TupleSections #-}

module DefaultEnv (defaultEnv) where

import Scheme
import Eval
import Data.Map (fromList)
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad ((=<<))
import Control.Monad.Trans (lift)
import Data.IORef

nil :: Value
nil = SExp (List [])

unwrapSyms :: [Expr] -> String -> ExceptT Error IO [Name]
unwrapSyms [] _ = return []
unwrapSyms ((Atom (Symbol s)):xs) n = (s :) <$> unwrapSyms xs n
unwrapSyms (x:xs) n = throwError $ WrongArgType n SymbolType (getExprType x)

scmDefine envRef [Atom (Symbol s), v] = do
    res <- eval envRef v
    ref <- lift $ newIORef res
    env <- lift $ readIORef envRef
    lift $ writeIORef envRef (M.insert s ref env)
    return nil
scmDefine envRef (Atom (Symbol s):xs) = throwError $ WrongNumArgs "define" (length xs + 1) (2, Just 2)
scmDefine envRef (List xs:body) = do
    syms <- unwrapSyms xs "define"
    ref  <- lift $ newIORef $ Func (head syms) (tail syms) body envRef
    env  <- lift $ readIORef envRef
    lift $ writeIORef envRef (M.insert (head syms) ref env)
    return nil

scmLambda :: SpecialForm
scmLambda envRef ((List xs):body) = do
    syms <- unwrapSyms xs "lambda"
    return $ Func "anonymous-lambda" syms body envRef
scmLambda envRef (x:_)            = throwError $ WrongArgType "lambda" ListType (getExprType x)

scmSet :: SpecialForm
scmSet envRef [Atom (Symbol s), v] = do
    env <- lift $ readIORef envRef
    case M.lookup s env of
      Just ref -> do val <- eval envRef v 
                     lift $ writeIORef ref val
                     return nil
      Nothing  -> throwError $ NotFound s
scmSet envRef [x, _] = throwError $ WrongArgType "set!" SymbolType (getExprType x)

scmIf envRef [x, a, b] = do
    cond <- eval envRef x
    if truthy cond
      then eval envRef a
      else eval envRef b
  where truthy (Number n)       = n /= 0
        truthy (Bool b)         = b
        truthy (SExp (List [])) = False
        truthy _                = True

scmDisplay envRef [x] = do
    lift $ print x
    return $ SExp (List [])

scmEval envRef [SExp exp] = eval envRef exp
scmEval envRef [v]        = throwError $ WrongArgType "eval" SExpType (getType v)

scmBegin envRef xs = evalExprs envRef xs

unwrapNums :: [Value] -> String -> ExceptT Error IO [Double]
unwrapNums [] _ = return []
unwrapNums ((Number x):xs) n = (x :) <$> unwrapNums xs n
unwrapNums (x:xs) n = throwError $ WrongArgType n NumberType (getType x)

binOp :: String -> (Double -> Double -> Double) -> Double -> Value
binOp n f d = Prim n (0, Nothing) op
  where op _ xs
          | null xs   = return $ Number d
          | otherwise = (\xs -> Number $ foldl f (head xs) (tail xs)) <$> unwrapNums xs n

numCompOp :: String -> (Double -> Double -> Bool) -> Value
numCompOp n f = Prim n (2, Just 2) op
  where op _ xs = (\[x,y] -> Bool $ f x y) <$> unwrapNums xs n

scmCar :: Primitive
scmCar _ [SExp (List [])] = throwError $ WrongArgType "car" ListType NilType
scmCar _ [SExp (List xs)] = return $ toValue (head xs)
  where toValue (List xs)         = SExp (List xs) 
        toValue (Atom (Symbol s)) = SExp (Atom $ Symbol s)
        toValue (Atom v)          = v
scmCar _ [v]              = throwError $ WrongArgType "car" ListType (getType v)

scmCdr :: Primitive
scmCdr _ [SExp (List [])] = throwError $ WrongArgType "cdr" ListType NilType
scmCdr _ [SExp (List xs)] = return $ SExp $ List (tail xs)
scmCdr _ [v]              = throwError $ WrongArgType "cdr" ListType (getType v)

scmCons :: Primitive
scmCons _ [v, SExp (List xs)] = return $ SExp (List ((toExpr v):xs))
  where toExpr (SExp (List xs)) = List xs
        toExpr (SExp (Atom v))  = Atom v
        toExpr v                = Atom v
scmCons _ [_, v]              = throwError $ WrongArgType "cons" ListType (getType v)

scmNull _ [SExp (List [])] = return $ Bool True
scmNull _ _                = return $ Bool False

defaultEnv :: IO (M.Map Name (IORef Value))
defaultEnv = fmap fromList $ mapM (\(x, y) -> fmap (x,) (newIORef y)) $ pairs
  where pairs = [ ("define", Form "define" (2, Nothing) scmDefine)
                , ("lambda", Form "lambda" (2, Nothing) scmLambda)
                , ("set!", Form "set!" (2, Nothing) scmSet)
                , ("begin", Form "begin" (1, Nothing) scmBegin)
                , ("if", Form "if" (3, Just 3) scmIf)
                , ("display", Prim "display" (1, Just 1) scmDisplay)
                , ("eval", Prim "eval" (1, Just 1) scmEval)
                , ("+", binOp "+" (+) 0)
                , ("-", binOp "-" (-) 0)
                , ("*", binOp "*" (*) 1)
                , ("/", binOp "/" (/) 1)
                , ("%", binOp "%" (\x y -> fromIntegral $ round x `mod` round y) 1)
                , (">", numCompOp ">" (>))
                , ("<", numCompOp "<" (<))
                , (">=", numCompOp ">=" (>=))
                , ("<=", numCompOp "<=" (<=))
                , ("car", Prim "car" (1, Just 1) scmCar)
                , ("cdr", Prim "cdr" (1, Just 1) scmCdr)
                , ("cons", Prim "cons" (2, Just 2) scmCons)
                , ("null?", Prim "null?" (1, Just 1) scmNull)
                ]
            

