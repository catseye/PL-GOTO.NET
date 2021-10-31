#!/bin/sh

APPLIANCES="tests/appliances/PLexceptGOTOdotNET.md"
ILASM=ilasm
if [ `which $ILASM`x = x ]; then
    echo "NOTE: $ILASM not found, skipping compiler tests."
else
    APPLIANCES="$APPLIANCES tests/appliances/PLexceptGOTOdotNET-loadngo.md"
fi
falderal $APPLIANCES tests/PLexceptGOTOdotNET.markdown || exit 1
