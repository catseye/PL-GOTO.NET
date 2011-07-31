#!/bin/sh
# Encoding: UTF-8

echo "n ← 0; LOOP n; m ← n; END;" > example.pl-g
ghc PLexceptGOTOdotNET.lhs -e 'compileFile "example.pl-g"' > example.msil
ilasm example.msil /output:example.exe
chmod 755 example.exe
./example.exe
