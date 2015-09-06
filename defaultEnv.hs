{-# LANGUAGE TupleSections #-}

module DefaultEnv (defaultEnv) where

import Scheme
import Eval
import Data.Map (fromList)
import qualified Data.Map as M (insert)
import Control.Monad.Except
import Control.Monad.Trans (lift)

nil :: Value
nil = SExp (List [])

unwrapSyms :: [Expr] -> String -> ExceptT Error IO [Name]
unwrapSyms [] _ = return []
unwrapSyms ((Atom (Symbol s)):xs) n = (s :) <$> unwrapSyms xs n
unwrapSyms (x:xs) n = throwError $ WrongArgType n (getExprType x) SymbolType

scmDefine env [Atom (Symbol s), v] = (nil,) <$> ((flip (M.insert s) env) <$> fmap fst (eval env v))
scmDefine env (Atom (Symbol s):xs) = throwError $ WrongNumArgs "define" (length xs + 1) (2, Just 2)
scmDefine env (List xs:body)      = do
    syms <- unwrapSyms xs "define"
    return (nil, M.insert (head syms) (Func (head syms) (tail syms) body) env)

scmLambda :: SpecialForm
scmLambda env ((List xs):body) = do
    syms <- unwrapSyms xs "lambda"
    return (Func "anonymous lambda" syms body, env)
scmLambda env (x:_)            = throwError $ WrongArgType "lambda" (getExprType x) ListType

scmIf env [x, a, b] = do
    cond <- fst <$> eval env x
    if truthy cond
      then eval env a
      else eval env b
  where truthy (Number n)       = n /= 0
        truthy (Bool b)         = b
        truthy (SExp (List [])) = False
        truthy _                = True

scmDisplay env [x] = do
    lift $ print x
    return $ SExp (List [])

scmEval env [SExp exp] = fst <$> eval env exp
scmEval env [v]        = throwError $ WrongArgType "eval" (getType v) SExpType

scmBegin env xs = evalExprs env xs

unwrapNums :: [Value] -> String -> ExceptT Error IO [Double]
unwrapNums [] _ = return []
unwrapNums ((Number x):xs) n = (x :) <$> unwrapNums xs n
unwrapNums (x:xs) n = throwError $ WrongArgType n (getType x) NumberType

binOp :: String -> (Double -> Double -> Double) -> Double -> Value
binOp n f d = Prim n (0, Nothing) op
  where op _ xs
          | null xs   = return $ Number d
          | otherwise = (\xs -> Number $ foldl f (head xs) (tail xs)) <$> unwrapNums xs n

numCompOp :: String -> (Double -> Double -> Bool) -> Value
numCompOp n f = Prim n (2, Just 2) op
  where op _ xs = (\[x,y] -> Bool $ f x y) <$> unwrapNums xs n

scmCar :: Primative
scmCar _ [SExp (List [])] = throwError $ WrongArgType "car" ListType NilType
scmCar _ [SExp (List xs)] = return $ toValue (head xs)
  where toValue (List xs)         = SExp (List xs) 
        toValue (Atom (Symbol s)) = SExp (Atom $ Symbol s)
        toValue (Atom v)          = v
scmCar _ [v]              = throwError $ WrongArgType "car" ListType (getType v)

scmCdr :: Primative
scmCdr _ [SExp (List [])] = throwError $ WrongArgType "cdr" ListType NilType
scmCdr _ [SExp (List xs)] = return $ SExp $ List (tail xs)
scmCdr _ [v]              = throwError $ WrongArgType "cdr" ListType (getType v)

defaultEnv :: Env
defaultEnv = fromList $
               [ ("define", Form "define" (2, Nothing) scmDefine)
               , ("lambda", Form "lambda" (2, Nothing) scmLambda)
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
               ]
            

