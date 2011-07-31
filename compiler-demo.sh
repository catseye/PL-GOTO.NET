#!/bin/sh
# Encoding: UTF-8

#echo "n ← 0; LOOP n; m ← n; END;" > example.pl-g
#echo "n ← 0; m ← n + 1; n ← m + 1;" > example.pl-g
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
ghc PLexceptGOTOdotNET.lhs -e 'compileFile "example.pl-g"' > example.msil
ilasm example.msil /output:example.exe
chmod 755 example.exe
./example.exe
