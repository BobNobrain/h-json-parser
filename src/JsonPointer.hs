-- rfc 6901
module JsonPointer
    ( JsonPointer (..)
    , getJsonValue
    , fromString
    )where

import Json
import Text.ParserCombinators.Parsec
import Numeric (readDec)

newtype JsonPointer = JsonPointer [String] deriving (Eq)

fromString :: String -> Either ParseError JsonPointer
fromString = parse jPointer "(json pointer)"

jPointer :: GenParser Char st JsonPointer
jPointer = do
    tokens <- many $ char '/' >> refToken
    return $ JsonPointer tokens
    where
        refToken :: GenParser Char st String
        refToken = many $ choice [escaped, unescaped]

        escaped :: GenParser Char st Char
        escaped = do
            _ <- char '~'
            c <- oneOf "01"
            case c of '0' -> return '~'
                      '1' -> return '/'

        unescaped = noneOf "/~"

getJsonValue :: JSON -> JsonPointer -> Maybe JSON

getJsonValue obj (JsonPointer []) = Just obj

getJsonValue (JObj pairs) (JsonPointer (x:xs)) = processNext $ findValue pairs x where
    processNext :: Maybe JSON -> Maybe JSON
    processNext Nothing = Nothing
    processNext (Just j) = getJsonValue j $ JsonPointer xs
    findValue :: [(String, JSON)] -> String -> Maybe JSON
    findValue [] _ = Nothing
    findValue ((key, value):xs) query =
        if key == query then
            Just value
        else
            findValue xs query

getJsonValue (JArr vals) (JsonPointer (x:xs)) = processNext $ findValue vals x where
    processNext :: Maybe JSON -> Maybe JSON
    processNext Nothing = Nothing
    processNext (Just j) = getJsonValue j $ JsonPointer xs
    findValue :: [JSON] -> String -> Maybe JSON
    findValue list index = item where
        parsed = readDec index
        item = case parsed of [(n, str)] -> getJsonValue (list !! n) $ JsonPointer xs
                              _ -> Nothing

getJsonValue _ _ = Nothing
