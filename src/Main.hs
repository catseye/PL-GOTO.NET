module Main where

import System.Environment
import PLexceptGOTOdotNET

main = do
    args <- getArgs
    case args of
        ["parse", fileName] ->
            workOnFile pa fileName
        ["labelloops", fileName] ->
            loopLabelFile fileName
        ["interpret", fileName] ->
            runFile fileName
        ["translate", fileName] ->
            compileFile fileName
        _ ->
            putStrLn "usage: PLexceptGOTOdotNET (parse|labelloops|interpret|translate) filename.pl-g"
