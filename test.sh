#!/bin/sh

echo "Building..."
./build.sh || exit 1
echo "Testing frontend and interpreter..."
falderal test tests/PLexceptGOTOdotNET.markdown || exit 1
if [ `which ilasm`x = x ]; then
    echo "ilasm not found, skipping compiler tests."
else
    echo "Testing compiler..."
    falderal test tests/PLexceptGOTOdotNET-run.markdown || exit 1
fi
