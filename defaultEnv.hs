{-# LANGUAGE TupleSections #-}

module DefaultEnv (defaultEnv) where

import Scheme
import Eval
import Data.Map (fromList)
import qualified Data.Map as M (insert)
import Control.Monad.Except
import Control.Monad.Trans (lift)

scmDefine env [Atom (Symbol s), v] = (SExp $ List [],) <$> flip (M.insert s) env <$> fmap fst (eval env v)
scmDefine env [x, _]               = throwError $ WrongArgType "define" (getExprType x) SymbolType

scmIf env [x, a, b] = do
    cond <- fst <$> eval env x
    if truthy cond
      then eval env a
      else eval env b
  where truthy (Number n)       = n /= 0
        truthy (SExp (List [])) = False
        truthy _                = True

scmDisplay env [x] = do
    lift $ print x
    return $ SExp (List [])

scmEval env [SExp exp] = fst <$> eval env exp
scmEval env [v]        = throwError $ WrongArgType "eval" (getType v) SExpType

unwrapNums :: [Value] -> ExceptT Error IO [Double]
unwrapNums [] = return []
unwrapNums ((Number n):xs) = (n :) <$> unwrapNums xs
unwrapNums (x:xs) = throwError $ WrongArgType "operator" (getType x) NumberType

binOp :: String -> (Double -> Double -> Double) -> Double -> Value
binOp n f d = Prim n (0, Nothing) op
  where op _ xs
          | null xs   = return $ Number d
          | otherwise = (\xs -> Number $ foldl f (head xs) (tail xs)) <$> unwrapNums xs

defaultEnv :: Env
defaultEnv = fromList $
               [ ("define", Form "define" (2, Just 2) scmDefine)
               , ("if", Form "if" (3, Just 3) scmIf)
               , ("display", Prim "display" (1, Just 1) scmDisplay)
               , ("eval", Prim "eval" (1, Just 1) scmEval)
               , ("+", binOp "+" (+) 0)
               , ("-", binOp "-" (-) 0)
               , ("*", binOp "*" (*) 1)
               , ("/", binOp "/" (/) 1)
               , ("%", binOp "%" (\x y -> fromIntegral $ round x `mod` round y) 1)
               ]
            

