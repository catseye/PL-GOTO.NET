module Main where

import System.Environment
import PLexceptGOTOdotNET

main = do
    [fileName] <- getArgs
    c <- readFile fileName
    putStrLn $ run c
