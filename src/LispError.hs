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

instance Show LispError where show = err

asArray :: [LispVal] -> String
asArray args = "[" ++ unwordsList args ++ "]"

err :: LispError -> String
err (Unbound msg x)  = [fmt|{msg}: {x}|]
err (BadForm msg x)  = [fmt|{msg}: {show x}|]
err (NotFunc msg f)  = [fmt|{msg}: {show f}|]
err (ParseErr e)     = [fmt|Parse error at {show e}|]
err (TypeErr exp t)  = [fmt|Invalid type: expected {exp}, found {show t}|]
err (NumArgs n args) = [fmt|Expected {n} args; found {asArray args}|]

type ThrowsError = Either LispError

-- What would be the type of this function?
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
