module Main where

import Lexer
import Parser
import Translator

import System.Environment
import System.FilePath

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> do
            let outputFile = replaceExtension inputFile ".lp"
            contents <- readFile inputFile
            let tokens = scanTokens contents
            writeFile outputFile (concatsep (translateLPOD (parseLPOD tokens)) "\n" ++ "\n")
        _ -> putStrLn "Usage: ./lpod2asp <source.lpod>"