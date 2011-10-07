#!/bin/sh
if [ "${FALDERAL}x" = "x" ]; then
    FALDERAL=../falderal/
fi
ghc -i$FALDERAL $FALDERAL/Test/Falderal/Driver.hs -e 'format "haskell" "PLexceptGOTOdotNET.falderal"' > GeneratedFalderalTests.hs
ghc -i$FALDERAL GeneratedFalderalTests.hs -e testModule
