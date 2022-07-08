module Parser (readExpr) where

import           Control.Monad.Except          (throwError)
import           Evaluator                     (eval)
import           LispError                     (LispError (..), ThrowsError)
import           LispVal                       (LispVal (..))
import qualified Text.ParserCombinators.Parsec as PS
import           Text.ParserCombinators.Parsec ((<|>))

spaces :: PS.Parser ()
spaces = PS.skipMany1 PS.space

symbol :: PS.Parser Char
symbol = PS.oneOf "!#$%&|*+-/:<=>?@^_~"

-- TODO: add support for escaped characters (\", \n, \r, \t, \\)
parseString :: PS.Parser LispVal
parseString = do
    PS.char '"'
    x <- PS.many (PS.noneOf "\"")
    PS.char '"'
    return $ String x

parseAtom :: PS.Parser LispVal
parseAtom = do
    first <- PS.letter <|> symbol
    rest  <- PS.many (PS.letter <|> PS.digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "true"  -> Bool True
        "false" -> Bool False
        _       -> Atom atom

parseNumber :: PS.Parser LispVal
parseNumber = Number . read <$> PS.many1 PS.digit

parseList :: PS.Parser LispVal
parseList = List <$> PS.sepBy parseExpr spaces

-- I'm not sure what this is, but it's a LISP/Scheme thing. Do I really need it?
parseDotList :: PS.Parser LispVal
parseDotList = do
    head <- PS.endBy parseExpr spaces
    tail <- PS.char '.' >> spaces >> parseExpr
    return $ DotList head tail

parseOuterList :: PS.Parser LispVal
parseOuterList = do
    PS.char '('
    x <- PS.try parseList <|> parseDotList
    PS.char ')'
    return x

-- TODO: add support for backquote (quasiquote/unquote)
parseQuoted :: PS.Parser LispVal
parseQuoted = do
    PS.char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: PS.Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> parseOuterList

readExpr :: String -> ThrowsError LispVal
readExpr input = case PS.parse parseExpr "lisp" input of
    Left  err -> throwError $ LispError.ParseErr err
    Right val -> return val
