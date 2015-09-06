module Scheme where

import Control.Monad.Except
import qualified Data.Map as M
import Data.List (intercalate)

type Name = String
type NumArgs = (Int, Maybe Int)

data Error = WrongArgType Name Type Type
           | WrongNumArgs Name Int (Int, Maybe Int)
           | NotFound Name
           | EmptyList deriving (Eq, Show)

data Type = NumberType
          | BoolType
          | SymbolType
          | SExpType 
          | FuncType
          | PrimType
          | FormType 
          | ListType 
          | NilType deriving (Eq, Show)

type Primative = (Env -> [Value] -> ExceptT Error IO Value)
type SpecialForm = (Env -> [Expr] -> ExceptT Error IO (Value, Env))

data Value = Number Double
           | Bool Bool
           | Symbol Name
           | SExp Expr
           | Func Name [Name] [Expr]
           | Prim Name (Int, Maybe Int) Primative
           | Form Name (Int, Maybe Int) SpecialForm

instance Show Value where
    show (Number n)
        | isInt n   = show (floor n)
        | otherwise = show n
        where isInt n = n == fromIntegral (floor n)
    show (Symbol s)      = s
    show (Bool b)        = if b then "#t" else "#f"
    show (SExp exp)      = show exp
    show (Func n args _) = "<function (" ++ n ++ " " ++ intercalate " " args ++ ")>"
    show (Prim n _ _)    = "<primative " ++ n ++ ">"
    show (Form n _ _)    = "<special-form " ++ n ++ ">"

getType :: Value -> Type
getType (Number _)   = NumberType
getType (Bool _)     = BoolType
getType (Symbol _)   = SymbolType
getType (SExp _)     = SExpType
getType (Func _ _ _) = FuncType
getType (Prim _ _ _) = PrimType
getType (Form _ _ _) = FormType

data Expr = List [Expr] | Atom Value 

instance Show Expr where
    show (List xs) = "(" ++ intercalate " " (map show xs) ++ ")"
    show (Atom v)  = show v

getExprType (Atom v)  = getType v
getExprType (List []) = NilType
getExprType (List xs) = ListType

type Env = M.Map Name Value

type Evaluator = ExceptT Error IO (Value, Env)

