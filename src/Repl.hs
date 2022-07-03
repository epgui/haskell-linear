module Repl where

import Control.Monad (unless)
import System.IO

read_ :: IO String
read_ = putStr "REPL> "
     >> hFlush stdout
     >> getLine

eval_ :: String -> String
eval_ input = input

print_ :: String -> IO ()
print_ = putStrLn

repl :: IO ()
repl = do
  input <- read_
  unless (input == ":quit")
       $ print_ (eval_ input)
      >> repl
