import Repl
import DefaultEnv
import System.Environment (getArgs)
import Eval
import Parse
import Text.ParserCombinators.Parsec (parse)

main = do
    args <- getArgs
    case args of
      []  -> repl defaultEnv
      [f] -> do
        xs <- readFile f
        res <- evalStr defaultEnv f xs
        case res of
          Right val -> return ()
          Left err  -> putStrLn err

