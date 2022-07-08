module LispVal (
    LispVal(..),
    unwordsList
) where

-- TODO: add support for floats / decimals (see readFloat)
-- TODO: add support for vectors (Array?)
data LispVal = Atom String
             | List [LispVal]
             | DotList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String a)    = "\"" ++ a ++ "\""
showVal (Atom a)      = a
showVal (Number a)    = show a
showVal (Bool True)   = "true"
showVal (Bool False)  = "false"
showVal (List a)      = "(" ++ unwordsList a ++ ")"
showVal (DotList a b) = "(" ++ unwordsList a ++ " . " ++ showVal b ++ ")"
