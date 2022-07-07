module Evaluator (eval) where

import           Control.Monad.Except (throwError)
import           Data.Functor         ((<&>))
import           LispError            (LispError (..), ThrowsError)
import           LispVal              (LispVal (..))

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackStrNum :: String -> ThrowsError Integer
unpackStrNum n =
    let parsed = reads n in
        if null parsed
            then throwError $ TypeMismatch "number" $ String n
            else return $ fst $ head parsed

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = unpackStrNum n
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params <&> (Number . foldl1 op)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
    if length args /= 2
        then throwError $ NumArgs 2 args
        else do left  <- unpacker $ head args
                right <- unpacker $ args !! 1
                return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",         numericBinop (+)),
              ("-",         numericBinop (-)),
              ("*",         numericBinop (*)),
              ("/",         numericBinop div),
              ("mod",       numericBinop mod),
              ("quotient",  numericBinop quot),
              ("remainder", numericBinop rem),
              ("=",         numBoolBinop (==)),
              ("<",         numBoolBinop (<)),
              (">",         numBoolBinop (>)),
              ("/=",        numBoolBinop (/=)),
              (">=",        numBoolBinop (>=)),
              ("<=",        numBoolBinop (<=)),
              ("&&",        boolBoolBinop (&&)),
              ("||",        boolBoolBinop (||)),
              ("string=?",  strBoolBinop (==)),
              ("string<?",  strBoolBinop (<)),
              ("string>?",  strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args =
    maybe (throwError $ NotFunction "Unrecognized primitive function args" f)
          ($ args)
          (lookup f primitives)

evalIf :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal
evalIf pred a b = do
    result <- eval pred
    case result of
        Bool False -> eval b
        _          -> eval a

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)                 = return val
eval val@(Number _)                 = return val
eval val@(Bool _)                   = return val
eval (List [Atom "quote", val])     = return val
eval (List [Atom "if", pred, a, b]) = evalIf pred a b
eval (List (Atom fn : args))        = mapM eval args >>= apply fn
eval bad = throwError $ BadSpecialForm "Unrecognized special form" bad
