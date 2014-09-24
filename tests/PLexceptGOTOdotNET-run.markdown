Test Suite for PL-{GOTO}.NET
============================

This test suite is written in the format of Falderal 0.9.  It is far from
exhaustive, but provides a basic sanity check that the language we've
implemented comes close to PL-{GOTO}.

Compiling
---------

    -> Tests for functionality "Compile PL-{GOTO} to .NET Executable and Run it"

    -> Functionality "Compile PL-{GOTO} to .NET Executable and Run it"
    -> is implemented by shell command
    -> "bin/PLexceptGOTOdotNET-run %(test-body-file)"

(Ideally we should just re-use the tests above for "PL-{GOTO} Evaluation", but
unfortunately the programs produced by the compiler have a different output
syntax right now.  Also, the compiler puts all variables in the output
dictionary, with unassigned variables given the value 0, instead of being not
present in the output dictionary.)

    | n ← 0;
    = n=0

    | n ← 0; m ← n + 1; n ← m + 1;
    = m=1
    = n=2

    | n ← 0; LOOP n; m ← n; END;
    = m=0
    = n=0

    | n ← 0; n ← n + 1; LOOP n; m ← n; END;
    = m=1
    = n=1

    | m ← 0; n ← 0; n ← n + 1; n ← n + 1; LOOP n; m ← m + 1; END;
    = m=2
    = n=2

    | n ← 0; n ← n + 1; n ← n + 1; n ← n + 1; n ← n + 1;
    | m ← 0; k ← 0;
    | LOOP n;
    |     m ← m + 1;
    |     LOOP m;
    |         k ← k + 1;
    |     END;
    | END;
    = k=10
    = m=4
    = n=4

    | n ← 0; n ← n + 1; n ← n + 1; n ← n + 1; n ← n + 1;
    | m ← 0;
    | LOOP n;
    |     n ← 0; n ← n + 1;
    |     m ← m + 1;
    | END;
    = m=4
    = n=1
