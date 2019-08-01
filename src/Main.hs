module Main where

import System.Environment
import System.IO
import PLexceptGOTOdotNET

main = do
    args <- getArgs
    case args of
        ["parse", fileName] ->
            workOnFile pa fileName
        ["labelloops", fileName] ->
            workOnFile loopLabel fileName
        ["interpret", fileName] ->
            workOnFile run fileName
        ["translate", fileName] ->
            workOnFile compile fileName
        _ ->
            putStrLn "usage: PLexceptGOTOdotNET (parse|labelloops|interpret|translate) filename.pl-g"

workOnFile fn fileName = do
    handle <- openFile fileName ReadMode
    -- hSetEncoding handle utf8
    contents <- hGetContents handle
    outputText <- return $ fn contents
    putStrLn outputText
