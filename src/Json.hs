module Json
    ( JSON (..)
    ) where

import Data.List (intercalate)

data JSON
    = JNull
    | JBool Bool
    | JNum Double
    | JStr String
    | JArr [JSON]
    | JObj [(String, JSON)]
    deriving (Eq)

instance Show JSON where
    show JNull = "null"
    show (JBool b) = if b then "true" else "false"
    show (JNum n) = show n
    show (JStr s) = '"' : foldr theFold s [] ++ "\"" where
        theFold :: Char -> String -> String
        theFold item acc = escape item ++ acc
        escape :: Char -> String
        escape '"'      = "\\\""
        escape '\\'     = "\\\\"
        escape '/'      = "\\/"
        escape '\b'     = "\\b"
        escape '\f'     = "\\f"
        escape '\n'     = "\\n"
        escape '\r'     = "\\r"
        escape '\t'     = "\\t"
    show (JArr items) = "[" ++ intercalate ", " (map show items) ++ "]"
    show (JObj pairs) = "{" ++ intercalate ", " (map shpr pairs) ++ "}" where
        shpr :: (String, JSON) -> String
        shpr (key, value) = "\"" ++ key ++ "\": " ++ show value
