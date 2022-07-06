module Repl where

import           Control.Monad (unless)
import           Parser
import           System.IO

read_ :: IO String
read_ = putStr "haskell-linear> "
    >> hFlush stdout
    >> getLine

eval_ :: String -> String
eval_ = readExpr

print_ :: String -> IO ()
print_ = putStrLn

repl :: IO ()
repl = do
    input <- read_
    unless (input == ":quit") $
        print_ (eval_ input)
            >> repl
