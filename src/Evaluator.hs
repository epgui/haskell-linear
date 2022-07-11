module Evaluator (eval) where

import           Control.Monad.Except (catchError, throwError)
import           Data.Functor         ((<&>))
import           LispError            (LispError (..), ThrowsError)
import           LispVal              (LispVal (..))

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeErr "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeErr "string" notString

unpackStrNum :: String -> ThrowsError Integer
unpackStrNum n =
    let parsed = reads n in
        if null parsed
            then throwError $ TypeErr "number" $ String n
            else return $ fst $ head parsed

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = unpackStrNum n
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeErr "number" notNum

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []      = throwError $ NumArgs 2 []
numericBinop op val@[_] = throwError $ NumArgs 2 val
numericBinop op params  = mapM unpackNum params <&> (Number . foldl1 op)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
    if length args /= 2
        then throwError $ NumArgs 2 args
        else do
            left  <- unpacker $ head args
            right <- unpacker $ args !! 1
            return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]      = return x
car [DotList (x:xs) _] = return x
car [badArg]           = throwError $ TypeErr "pair" badArg
car badArgList         = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]      = return $ List xs
cdr [DotList [_] x]    = return x
cdr [DotList (_:xs) x] = return $ DotList xs x
cdr [badArg]           = throwError $ TypeErr "pair" badArg
cdr badArgList         = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]          = return $ List [x]
cons [x, List xs]          = return $ List $ x:xs
cons [x, DotList xs xlast] = return $ DotList (x:xs) xlast
cons [a, b]                = return $ DotList [a] b
cons badArgList            = throwError $ NumArgs 2 badArgList

eqvList :: [LispVal] -> ThrowsError LispVal
eqvList [List a, List b] =
    return $ Bool $ (length a == length b) && all eqvPair (zip a b)
    where
        eqvPair (a, b) = case eqv [a, b] of
            Left err         -> False
            Right (Bool val) -> val

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool a, Bool b]             = return $ Bool $ a == b
eqv [Number a, Number b]         = return $ Bool $ a == b
eqv [String a, String b]         = return $ Bool $ a == b
eqv [Atom a, Atom b]             = return $ Bool $ a == b
eqv [DotList xs x, DotList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [a@(List _), b@(List _)]     = eqvList [a, b]
eqv [_, _]                       = return $ Bool False
eqv badArgList                   = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) =
    do unpackedA <- unpacker a
       unpackedB <- unpacker b
       return $ unpackedA == unpackedB
    `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [a, b] = do
    primitiveEquals <- or <$> mapM (unpackEquals a b) [AnyUnpacker unpackNum,
                                                       AnyUnpacker unpackStr,
                                                       AnyUnpacker unpackBool]
    eqvEquals <- eqv [a, b]
    return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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
              ("string>=?", strBoolBinop (>=)),
              ("car",       car),
              ("cdr",       cdr),
              ("cons",      cons),
              ("eq?",       eqv),
              ("eqv?",      eqv),
              ("equal?",    equal)]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args =
    maybe (throwError $ NotFunc "Unrecognized primitive function args" f)
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
eval badForm = throwError $ BadForm "Unrecognized special form" badForm
