module Main where

import System.Environment
import PLexceptGOTOdotNET

main = do
    [action, fileName] <- getArgs
    case action of
        "interpret" ->
            runFile fileName
        "translate" ->
            compileFile fileName
        -- "compile" ->
        --     compileFile fileName
        --     -- ilasm tmp.msil /output:$3 >/dev/null
        --     -- chmod 755 $3
        --     -- rm tmp.msil
        -- "run" ->
        --     compileFile fileName
        --     -- ilasm tmp.msil /output:program.exe >/dev/null
        --     -- chmod 755 program.exe
        --     -- rm tmp.msil
        --     -- ./program.exe
        --     -- rm program.exe
        _ ->
            putStrLn "usage: PLexceptGOTOdotNET (interpret|translate|compile|run) filename.pl-g"
            -- usage:
            --     PLexceptGOTOdotNET interpret file.pl-g
            --     --- interpret a PL-{GOTO} program directly
            -- 
            --     PLexceptGOTOdotNET translate file.pl-g file.msil
            --     --- produce a CIL assembly source from a PL-{GOTO} program
            -- 
            --     PLexceptGOTOdotNET compile file.pl-g file.exe
            --     --- produce a .NET executable for a PL-{GOTO} program
            -- 
            --     PLexceptGOTOdotNET run file.pl-g
            --     --- produce a .NET executable for a program, then immediately run it
