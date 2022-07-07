module Main where

import           Control.Monad (unless)
import           Evaluator
import           LispVal
import           Parser
import           System.IO

promptForInput :: IO ()
promptForInput = putStr "haskell-linear> " >> hFlush stdout

printOutput :: String -> IO ()
printOutput = putStrLn

readEvalPrint :: String -> String
readEvalPrint = show . eval . readExpr

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
    input <- promptForInput >> getLine
    unless (input == ":quit") $
        fmap printOutput readEvalPrint input >> readEvalPrintLoop

main :: IO ()
main = readEvalPrintLoop
