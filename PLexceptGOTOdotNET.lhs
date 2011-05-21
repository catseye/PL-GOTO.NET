encoding: UTF-8

PL-{GOTO}.NET
=============

Written by Chris Pressey of Cat's Eye Technologies.  This work is
hereby placed into the public domain.

This Haskell module implements the programming language PL-{GOTO} from
Brainerd and Landweber's _Theory of Computation_ (1974; ISBN 0471095850).
PL-{GOTO} is a subset of the full language PL described in the book; it
has had the GOTO instruction removed.  The choice to implement this
variant was made because it is computationally more interesting:
PL-{GOTO} can express exactly the primitive recursive functions, and
thus PL-{GOTO} programs always terminate.  On the other hand, all
algorithms that can run in nondeterminstic exponential time -- which
includes almost all practical algorithms - can be expressed primitive
recursively (although their primitive recursive version may be
drastically less efficient.)

The ultimate goal of this project is to compile PL-{GOTO} to MSIL (thus
the name.)

> module PLexceptGOTOdotNET where

> import Text.ParserCombinators.Parsec
> import qualified Data.Map as Map

Grammar for the full (but not "extended") PL language, from the book:

    <letter> = A ∪ B ∪ ... ∪ Z
    <digit> = 0 ∪ 1 ∪ ... ∪ 9
    <name> = <letter> ∪ <name><digit> ∪ <name><letter>
    <assignment> = <name>←0 ∪ <name>←<name>+1 ∪ <name>←<name>
    <instruction> = <assignment> ∪ GOTO<name>
    <label instruction> = <name>:<instruction>; ∪ <instruction>;
    <loop> = LOOP<name>; ∪ <name>:LOOP<name>;
    <end> = END; ∪ <name>:END;
    <program> = <label instruction> ∪ <loop><program><end> ∪
                <program><program>

Sample code from the book:

    V ← 0;
    LOOP W;
        LOOP X;
            V ← V + 1;
        END;
        GOTO L;
    END;
    LOOP Y;
        V ← V + 1;
    END;
L:  LOOP Z;
        V ← V + 1;
    END;

Since we are implementing PL-{GOTO}, we can simplify the grammar; we do not
accept GOTO instructions or labels.  The "label instruction" production
basically collapses to "assignment".

    <letter> = A ∪ B ∪ ... ∪ Z
    <digit> = 0 ∪ 1 ∪ ... ∪ 9
    <name> = <letter> ∪ <name><digit> ∪ <name><letter>
    <assignment> = <name>←0 ∪ <name>←<name>+1 ∪ <name>←<name>
    <loop> = LOOP<name>;
    <end> = END;
    <program> = <assignment>; ∪ <loop><program><end> ∪ <program><program>

> type Name = String
> data Instruction = Block [Instruction]
>                  | Loop Name Instruction
>                  | AssignZero Name
>                  | AssignOther Name Name
>                  | AssignIncr Name Name
>    deriving (Eq, Show, Read)

> name :: Parser Name
> name = do
>     c <- letter
>     cs <- many alphaNum
>     spaces
>     return (c:cs)

> strspc s = do
>     string s
>     spaces

For fidelity with the grammar as it was originally presented, we are not
going to write this in an LL(1), predictive fashion.  Instead, each of our
productions is going to consume an entire assignment statement, one
of the three variants, and we're going to use `try` to backtrack.

> assignZero :: Parser Instruction
> assignZero = do
>     n <- name
>     strspc "←"
>     strspc "0"
>     return (AssignZero n)

> assignOther :: Parser Instruction
> assignOther = do
>     n <- name
>     strspc "←"
>     m <- name
>     return (AssignOther n m)

> assignIncr :: Parser Instruction
> assignIncr = do
>     n <- name
>     strspc "←"
>     m <- name
>     strspc "+"
>     strspc "1"
>     return (AssignIncr n m)

> loop :: Parser Instruction
> loop = do
>     strspc "LOOP"
>     n <- name
>     strspc ";"
>     p <- program
>     strspc "END"
>     return (Loop n p)

> program :: Parser Instruction
> program = do
>     l <- endBy1 (
>                   (try assignZero) <|>
>                   (try assignIncr) <|>
>                   (try assignOther) <|>
>                   (try loop)
>                 ) (strspc ";")
>     return (Block l)

> pa s = case parse program "" s of
>     Left perr -> show perr
>     Right prog -> show prog

Evaluator follows.  Sorry, no MSIL compiler yet :(

> store env name value = Map.insert name value env
> fetch env name =
>     let
>         Just value = Map.lookup name env
>     in
>         value

> eval env (Block []) = env
> eval env (Block (i:rest)) =
>     eval (eval env i) (Block rest)

Page 28: "Note that the above process does *not* change the
value of the LOOP variable.  The value of a variable may only be
changed by the execution of an assignment statement."

> eval env (Loop n i) =
>     loop (fetch env n) env i
>     where
>         loop 0 env i = env
>         loop count env i = loop (count-1) (eval env i) i
> eval env (AssignZero n) =
>     store env n 0
> eval env (AssignOther n m) =
>     store env n (fetch env m)
> eval env (AssignIncr n m) =
>     store env n ((fetch env m) + 1)

> run s = case parse program "" s of
>     Left perr -> show perr
>     Right prog -> show $ Map.toList $ eval Map.empty prog
