#!/bin/sh
# Encoding: UTF-8

# Run this script from the distribution's root directory.

ghc src/PLexceptGOTOdotNET.lhs -e 'compileFile "eg/example.pl-g"' > example.msil
ilasm example.msil /output:example.exe
chmod 755 example.exe
./example.exe
