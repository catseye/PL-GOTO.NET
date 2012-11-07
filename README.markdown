PL-{GOTO}.NET
=============

This is the reference distribution of PL-{GOTO}.NET, a literate Haskell
program which compiles a program written in PL-{GOTO} to an MSIL file which
can then be assembled by `ilasm` to produce a .NET executable.  PL-{GOTO}
is a pedagogical language, described in Brainerd and Landweber's _Theory of
Computation_ (1974; ISBN 0471095850), in which it is possible to express
only computations which are primitive recursive.

The source code of the compiler is in the file `PLexceptGOTOdotNET.lhs` in
the `src` directory of the distribution.  Being written in literate Haskell,
it also serves as documentation for the compiler.

A test suite in Falderal format can be found in the `tests` directory and
can be run with the `test.sh` script in the root directory.

A script that demonstrates the end-to-end usage of the compiler can be
found in the `demo` directory (but it should be run directly from the root
directory.)

The materials in this distribution are in the public domain; see the file
`UNLICENSE` for more information.
