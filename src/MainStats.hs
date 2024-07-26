module Main where

import Lexer
import Parser
import Stats

import System.Environment
import System.FilePath

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> do
            let outputFile = replaceExtension inputFile ".stats"
            contents <- readFile inputFile
            let tokens = scanTokens contents
            writeFile outputFile (concatsep (getStats (parseLPOD tokens)) " " ++ "\n")
        _ -> putStrLn "Usage: ./lpod2asp <source.lpod>"