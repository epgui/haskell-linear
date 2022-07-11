module Main where

import           Control.Monad (unless)
import           Evaluator     (eval)
import           LispError     (extractValue, trapError)
import           Parser        (readExpr)
import           System.IO     (hFlush, stdout)

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
    putStr "haskell-linear> "
    hFlush stdout
    input <- getLine
    unless (input == ":quit") $ do
        let evaled = fmap show $ readExpr input >>= eval
        putStrLn (extractValue $ trapError evaled)
        readEvalPrintLoop

main :: IO ()
main = readEvalPrintLoop
