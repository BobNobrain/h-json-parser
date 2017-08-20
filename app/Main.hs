module Main where

import System.Environment
import System.IO
import Control.Monad
import Parser
import Json
import JsonPointer
import Text.ParserCombinators.Parsec (ParseError)

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then
        putStrLn "Not enough arguments"
    else do
        let (filepath:jpath:_) = args
        eitherJson <- readJson filepath
        let eitherPath = fromString jpath
        doTheJob eitherJson eitherPath
        where
            doTheJob :: Either ParseError JSON -> Either ParseError JsonPointer -> IO ()
            doTheJob (Left err) _ = putStrLn $ "wrong json:" ++ show err
            doTheJob _ (Left err) = putStrLn $ "wrong json pointer" ++ show err
            doTheJob (Right json) (Right pointer) = do
                let maybeResult = getJsonValue json pointer
                case maybeResult of Nothing -> putStrLn "Specified pointer does not exist"
                                    Just selectedJson -> putStrLn $ show selectedJson

readJson :: String -> IO (Either ParseError JSON)
readJson p = do
    fcontent <- readFile p
    return $ parseJSON fcontent

