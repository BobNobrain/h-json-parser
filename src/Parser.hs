module Parser
    ( JSON
    , parseJSON
    ) where

import Text.ParserCombinators.Parsec
import Data.Char (chr)
import Numeric (readHex)


data JSON
    = JNull
    | JBool Bool
    | JNum Double
    | JStr String
    | JArr [JSON]
    | JObj [(String, JSON)]
    deriving (Show, Eq)

data JString
    = JStringEmpty
    | JChars String
    | JEscapedChar Char

parseJSON :: String -> Either ParseError JSON
parseJSON = parse jsonValue "(input)"

jsonValue :: GenParser Char st JSON
jsonValue = do
    _ <- spaces
    r <- choice [jsonKeyword, jsonNumber, jsonString, jsonObject, jsonArray]
    _ <- spaces
    return r


jsonKeyword :: GenParser Char st JSON
jsonKeyword = do
    result <- choice [string "null", string "true", string "false"]
    return $ cvt result where
        cvt "null" = JNull
        cvt "false" = JBool False
        cvt "true" = JBool True

jsonNumber :: GenParser Char st JSON
jsonNumber = do
    isNegative <- optionalMinus
    wholePart <- wholePartRule
    hasFractional <- optionMaybe $ char '.'
    fractionalPart <- case hasFractional of Nothing -> return 0.0
                                            Just '.' -> fracPartRule

    hasExp <- optionMaybe $ choice [char 'e', char 'E']
    exponentialPart <- case hasExp of Nothing -> return 0.0
                                      Just _ -> expPartRule

    return $ constructNumber isNegative wholePart fractionalPart exponentialPart
    where
        constructNumber :: Bool -> Integer -> Double -> Double -> JSON
        constructNumber isNegative wholePart fractionalPart exponentialPart = JNum n where
            n = if isNegative then (-n1) else n1
            n1 = n2 * (10.0 ** exponentialPart)
            n2 :: Double
            n2 = fractionalPart + fromInteger wholePart

        optionalMinus = do
            r <- optionMaybe $ char '-'
            case r of Nothing -> return False
                      Just '-' -> return True

        wholePartRule = do
            first <- choice [char '0', oneOf ['1'..'9']]
            rest <- many digit
            return $ read (first:rest)

        fracPartRule = do
            digits <- many digit
            return $ read $ "0." ++ digits

        expPartRule = do
            signMb <- optionMaybe $ choice [char '-', char '+']
            digits <- many digit
            return $ ct signMb digits where
                ct :: Maybe Char -> String -> Double
                ct Nothing ds = read ds
                ct (Just '-') ds = -read ds
                ct (Just '+') ds = read ds

jsonString :: GenParser Char st JSON
jsonString = do
    _ <- char '"'
    strC <- strContent
    _ <- char '"'
    return $ JStr strC
    where
        unescapedChar = noneOf ['"', '\\']
        escapedChar = do
            _ <- char '\\'
            c <- oneOf ['"', '\\', '/', 'b', 'f', 'n', 'r', 't', 'u']
            case c of 'u' -> do
                                code <- count 4 $ oneOf $ ['0'..'9'] ++ ['A'..'F']
                                return $ (chr . fst . head . readHex) code
                      _ -> return $ cvt c
                           where
                               cvt :: Char -> Char
                               cvt 'b' = '\b'
                               cvt 'f' = '\f'
                               cvt 'n' = '\n'
                               cvt 'r' = '\r'
                               cvt 't' = '\t'
                               cvt c = c


        strContent = many $ choice [escapedChar, unescapedChar]

jsonArray :: GenParser Char st JSON
jsonArray = do
    _ <- char '['
    _ <- spaces
    content <- sepBy jsonValue $ spaces >> char ',' >> spaces
    _ <- spaces
    _ <- char ']'
    return $ JArr content

jsonObject :: GenParser Char st JSON
jsonObject = do
    _ <- char '{'
    _ <- spaces
    pairs <- sepBy kvPair $ spaces >> char ',' >> spaces
    _ <- spaces
    _ <- char '}'
    return $ JObj pairs
    where
        kvPair = do
            key <- jsonString
            _ <- spaces
            _ <- char ':'
            _ <- spaces
            value <- jsonValue
            return (extract key, value)

        extract :: JSON -> String
        extract (JStr str) = str
