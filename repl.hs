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
    case parse parseExpr "<repl>" xs of
      Left err  -> putStrLn ("ERROR: " ++ show err) >> repl env
      Right exp -> do
        res <- runExceptT $ eval env exp
        case res of
         Right (val, newEnv) -> print val >> repl newEnv
         Left err            -> print err >> repl env
        

