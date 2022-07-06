module Repl where

import           Control.Monad (unless)
import           Evaluator
import           LispVal
import           Parser
import           System.IO

read_ :: IO String
read_ = putStr "haskell-linear> "
    >> hFlush stdout
    >> getLine

print_ :: String -> IO ()
print_ = putStrLn

repl :: IO ()
repl = do
    input <- read_
    unless (input == ":quit") $
        print_ ((show . eval . readExpr) input)
            >> repl
