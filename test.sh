#!/bin/sh

# This assumes Falderal 0.4 is installed.  The easiest way to do this is to
# install it as a Cabal package:
#   $ hg clone https://bitbucket.org/catseye/falderal
#   $ cd falderal
#   $ cabal install --prefix=$HOME --user

falderal test standard PLexceptGOTOdotNET.falderal
