> -- encoding: UTF-8
> module Tests where

> import qualified Test.Falderal.Runner as Runner
> import PLexceptGOTOdotNET

> test = Runner.run ["Tests.lhs"] [] [
>              ("PL-{GOTO} Parsing", pa),
>              ("PL-{GOTO} Evaluation", run)
>            ]

PL-{GOTO} Parsing
-----------------

| n ← 0;
= Block [AssignZero "n"]

| n ← m;
= Block [AssignOther "n" "m"]

| n ← n + 1;
= Block [AssignIncr "n" "n"]

| n ← 0; LOOP n; m ← n; END;
= Block [AssignZero "n",Loop "n" (Block [AssignOther "m" "n"])]

It's perfectly valid to have variables called LOOP and END.

| LOOP ← 0; LOOP LOOP; LOOP ← LOOP + 1; END;
= Block [AssignZero "LOOP",Loop "LOOP" (Block [AssignIncr "LOOP" "LOOP"])]

| END ← 0; LOOP END; END ← END + 1; END;
= Block [AssignZero "END",Loop "END" (Block [AssignIncr "END" "END"])]

PL-{GOTO} Evaluation
--------------------

| n ← 0;
= [("n",0)]

| n ← 0; LOOP n; m ← n; END;
= [("n",0)]

| n ← 0; n ← n + 1; LOOP n; m ← n; END;
= [("m",1),("n",1)]
