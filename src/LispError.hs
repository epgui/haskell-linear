module LispError (
    LispError(..),
    ThrowsError,
    extractValue,
    trapError
) where

import           Control.Monad.Except                (catchError)
import           LispVal                             (LispVal, unwordsList)
import           PyF                                 (fmt)
import           Text.ParserCombinators.Parsec.Error (ParseError)

data LispError = NumArgs Integer [LispVal]
               | TypeErr String LispVal
               | ParseErr ParseError
               | BadForm String LispVal
               | NotFunc String String
               | Unbound String String
               | Default String

instance Show LispError where show = showErr

formatArray :: [LispVal] -> String
formatArray args = "[" ++ unwordsList args ++ "]"

showErr :: LispError -> String
showErr (Unbound msg x)  = [fmt|{msg}: {x}|]
showErr (BadForm msg x)  = [fmt|{msg}: {show x}|]
showErr (NotFunc msg f)  = [fmt|{msg}: {show f}|]
showErr (ParseErr e)     = [fmt|Parse error at {show e}|]
showErr (TypeErr exp t)  = [fmt|Invalid type: expected {exp}, found {show t}|]
showErr (NumArgs n args) = [fmt|Expected {n} args; found {formatArray args}|]

type ThrowsError = Either LispError

-- What would be the type of this function?
trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
