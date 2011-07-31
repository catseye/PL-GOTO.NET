#!/bin/sh
if [ "${FALDERAL}x" = "x" ]; then
    FALDERAL=../falderal/
fi
ghc -i$FALDERAL Tests.lhs -e test
