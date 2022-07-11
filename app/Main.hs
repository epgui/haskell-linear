module Main where

import           Control.Monad (unless)
import           Evaluator     (eval)
import           LispError     (extractValue, trapError)
import           Parser        (readExpr)
import           System.IO     (hFlush, stdout)

promptForInput :: IO ()
promptForInput = putStr "haskell-linear> " >> hFlush stdout

printOutput :: String -> IO ()
printOutput = putStrLn

readEvalPrint :: String -> String
readEvalPrint input = do
    let evaled = fmap show $ readExpr input >>= eval
    extractValue $ trapError evaled

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
    promptForInput
    input <- getLine
    unless (input == ":quit") $
        fmap printOutput readEvalPrint input >> readEvalPrintLoop

main :: IO ()
main = readEvalPrintLoop
