#!/bin/sh
# Encoding: UTF-8

# Run this script from the distribution's root directory.

cat >example.pl-g <<EOF
n ← 0; n ← n + 1; n ← n + 1; n ← n + 1; n ← n + 1;
m ← 0; k ← 0;
LOOP n;
    m ← m + 1;
    LOOP m;
        k ← k + 1;
    END;
END;
EOF
#echo "n ← 0; m ← n + 1; n ← m + 1;" > example.pl-g
#echo "n ← 0; LOOP n; m ← n + 1; END;" > example.pl-g
ghc src/PLexceptGOTOdotNET.lhs -e 'compileFile "example.pl-g"' > example.msil
ilasm example.msil /output:example.exe
chmod 755 example.exe
./example.exe
