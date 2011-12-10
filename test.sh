#!/bin/sh

# This assumes Falderal 0.4 is installed.  The easiest way to do this
# currently is to install it as a local Cabal package:
#   $ hg clone https://bitbucket.org/catseye/falderal
#   $ cd falderal
#   $ ./install.sh

falderal test PLexceptGOTOdotNET.falderal
