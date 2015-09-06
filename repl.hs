module Repl where

import System.IO (hFlush, stdout)
import Eval
import Parse
import Scheme
import Control.Monad.Except (runExceptT)
import Text.ParserCombinators.Parsec (parse)

repl :: Env -> IO ()
repl env = do
    putStr ">>> "
    hFlush stdout
    xs <- getLine
    let res = parse parseExpr "<repl>" xs
    case res of
      Left err  -> print err >> repl env
      Right exp -> do
        res <- runExceptT $ eval env exp
        case res of
          Left err -> print err >> repl env
          Right val -> print val >> repl env
        

