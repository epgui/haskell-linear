module Main where

import           Control.Monad (unless)
import           Evaluator     (eval)
import           LispError     (extractValue, trapError)
import           Parser        (readExpr)
import           System.IO     (hFlush, stdout)

readEval :: String -> String
readEval input = do
    let evaled = fmap show $ readExpr input >>= eval
    extractValue $ trapError evaled

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
    putStr "haskell-linear> "
    hFlush stdout
    input <- getLine
    unless (input == ":quit") $ do
        putStrLn (readEval input)
        readEvalPrintLoop

main :: IO ()
main = readEvalPrintLoop
