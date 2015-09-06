import Repl
import DefaultEnv
import System.Environment (getArgs)
import Eval
import Control.Monad.Trans (lift)
import Data.IORef

main = do
    args <- getArgs
    envRef <- defaultEnv >>= newIORef
    case args of
      []  -> repl envRef
      [f] -> do
        xs  <- readFile f
        res <- evalStr envRef f xs
        case res of
          Right _   -> repl envRef
          Left err  -> putStrLn err

