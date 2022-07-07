module LispError where

import           Control.Monad.Except
import           LispVal
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar msg varname)      = msg ++ ": " ++ varname
showError (BadSpecialForm msg form)     = msg ++ ": " ++ show form
showError (NotFunction msg f)           = msg ++ ": " ++ show f
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

type ThrowsError = Either LispError

-- What would be the type of this function?
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
