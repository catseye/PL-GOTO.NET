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

AST
---

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

Parser
------

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

Evaluator
---------

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

Compiler
--------

The compiler takes an abstract representation of a PL-{GOTO}
program and produces a string containing an MSIL program
(suitable as input to `ilasm`) which computes the same function,
modulo limitations like 32-bit integers.

TODO write this.  What follows is just rough notes.

* Boilerplate

prelude = """
.assembly PLexceptGOTOprogram {}

.method static public void main() il managed
{
    .entrypoint
    .maxstack 32 // to be computed by the compiler.  Just guess high for now
"""

* Gather all variables, allocate them

locals = """
    .locals init ([0] int32 n, // to be determined by the compiler.
                  [1] int32 i, // all variables used in program, here.
		  ...)
"""

* Count all loops, allocate loop vars, compute maximum stack depth

AssignZero:

    // *****************************************************
    // i <- 0
    // *****************************************************
    ldc.i4.0                    // load constant onto stack
    stloc.1                     // store to variable 1

AssignOther:

    // *****************************************************
    // i <- n
    // *****************************************************
    ldloc.0                     // load variable 0 to stack
    stloc.1                     // store to variable 1

AssignIncr:

    // *****************************************************
    // i <- n + 1
    // *****************************************************
    ldloc.0                     // load variable 0 to stack
    ldc.i4.1                    // load constant onto stack
    add
    stloc.1                     // store to variable 1

Loop:

    // *****************************************************
    // loop template:  each loop has its own id, x, and its own temp var
    // tempx <- loopvar
    // br.s LOOPX_CHECK
    // LOOPX_TOP:
    // <<loop body>>
    // tempx--;
    // LOOPX_CHECK:
    // is tempx > 0?  jump to LOOPX_TOP
    // *****************************************************

postlude = """
    ret
}
"""

