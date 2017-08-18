module Main where

import System.Environment
import Control.Monad
import Parser

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then
        putStrLn "Not enough arguments"
    else
        mapM_ putStrLn args
