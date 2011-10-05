encoding: UTF-8

Test Suite for PL-{GOTO}.NET
============================

This test suite is written in the format of Falderal 0.3.  It is far from
exhaustive, but provides a basic sanity check that the language we've
implemented comes close to PL-{GOTO}.

PL-{GOTO} Parsing
-----------------

-> Tests for Haskell function PLexceptGOTOdotNET:pa

| n ← 0;
= Block [AssignZero "n"]

| n ← m;
= Block [AssignOther "n" "m"]

| n ← n + 1;
= Block [AssignIncr "n" "n"]

| n ← 0; LOOP n; m ← n; END;
= Block [AssignZero "n",Loop 0 "n" (Block [AssignOther "m" "n"])]

| n ← 0; m ← 0;
| LOOP n;
|     m ← n + 1;
| END;
= Block [AssignZero "n",AssignZero "m",Loop 0 "n" (Block [AssignIncr "m" "n"])]

It's perfectly valid to have variables called LOOP and END.

| LOOP ← 0; LOOP LOOP; LOOP ← LOOP + 1; END;
= Block [AssignZero "LOOP",Loop 0 "LOOP" (Block [AssignIncr "LOOP" "LOOP"])]

| END ← 0; LOOP END; END ← END + 1; END;
= Block [AssignZero "END",Loop 0 "END" (Block [AssignIncr "END" "END"])]

PL-{GOTO} Evaluation
--------------------

-> Tests for Haskell function PLexceptGOTOdotNET:run

| n ← 0;
= [("n",0)]

| n ← 0; LOOP n; m ← n; END;
= [("n",0)]

| n ← 0; n ← n + 1; LOOP n; m ← n; END;
= [("m",1),("n",1)]

| m ← 0; n ← 0; n ← n + 1; n ← n + 1; LOOP n; m ← m + 1; END;
= [("m",2),("n",2)]

| n ← 0; n ← n + 1; n ← n + 1; n ← n + 1; n ← n + 1;
| m ← 0; k ← 0;
| LOOP n;
|     m ← m + 1;
|     LOOP m;
|         k ← k + 1;
|     END;
| END;
= [("k",10),("m",4),("n",4)]

Changing the value of a loop variable inside the loop does not change
the number of times the loop executes.

| n ← 0; n ← n + 1; n ← n + 1; n ← n + 1; n ← n + 1;
| m ← 0;
| LOOP n;
|     n ← 0; n ← n + 1;
|     m ← m + 1;
| END;
= [("m",4),("n",1)]

Loop Labeling
-------------

-> Tests for Haskell function PLexceptGOTOdotNET:testLoopLabeling

| n ← 0; m ← 0; LOOP n;
|     LOOP m;
|         n ← 0; 
|     END;
| END;
= (Block [AssignZero "n",AssignZero "m",Loop 1 "n" (Block [Loop 0 "m" (Block [AssignZero "n"])])],2)
