import Repl
import DefaultEnv
import System.Environment (getArgs)
import Eval
import Control.Monad.Trans (lift)

main = do
    args <- getArgs
    case args of
      []  -> defaultEnv >>= repl
      [f] -> do
        xs <- readFile f
        res <- defaultEnv >>= \x -> evalStr x f xs
        case res of
          Right (val, env) -> repl env
          Left err         -> putStrLn err

