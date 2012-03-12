encoding: UTF-8

PL-{GOTO}.NET
=============

Version 1.1

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
includes almost all practical algorithms -- can be expressed primitive
recursively (although their primitive recursive version may be
drastically less efficient.)

This module actually contains two implementations.  There is an interpreter
written in Haskell which evaluates PL-{GOTO} programs directly.  In addition
to this, there is a compiler which outputs MSIL code.  This output can be
fed into `ilasm` to produce an executable .NET assembly which performs the
same computation as the PL-{GOTO} program -- thus the name PL-{GOTO}.NET.
The compiler does no optimization whatsoever of the generated code.

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
>                  | Loop Integer Name Instruction
>                  | AssignZero Name
>                  | AssignOther Name Name
>                  | AssignIncr Name Name
>    deriving (Eq, Show, Read)

In the representation of the Loop instruction, above, the Integer is an
internal ID used by the compiler to uniquely identify the Loop in the program.
The parser doesn't care, and will just give every loop the ID 0.  Only later,
during static analysis, will these IDs be filled out.

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
>     return (Loop 0 n p)

> program :: Parser Instruction
> program = do
>     l <- endBy1 (
>                   (try assignZero) <|>
>                   (try assignIncr) <|>
>                   (try assignOther) <|>
>                   (try loop)
>                 ) (strspc ";")
>     return (Block l)

Drivers for the parser.

> pa s = case parse program "" s of
>     Left perr -> show perr
>     Right prog -> show prog

> parseFile fileName = do
>     programText <- readFile fileName
>     outputText <- return $ pa programText
>     putStrLn outputText

Environments
------------

An environment binds names to values.  This is just a perfunctory
abstraction around Haskell's Map datatype.

> empty = Map.empty
> toList = Map.toAscList
> singleton = Map.singleton
> store env name value = Map.insert name value env
> fetch env name =
>     let
>         Just value = Map.lookup name env
>     in
>         value
> register env name =
>     case Map.lookup name env of
>         Just value ->
>             env
>         Nothing ->
>             store env name 0

Evaluator
---------

The evaluator the environment that results from executing the given
PL-{GOTO} program given as an AST in the given environment.

> eval env (Block []) = env
> eval env (Block (i:rest)) =
>     eval (eval env i) (Block rest)

Page 28: "Note that the above process does *not* change the
value of the LOOP variable.  The value of a variable may only be
changed by the execution of an assignment statement."

> eval env (Loop _ n i) =
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

Drivers for the evaluator.

> run s = case parse program "" s of
>     Left perr -> show perr
>     Right prog -> show $ toList $ eval empty prog

> runFile fileName = do
>     programText <- readFile fileName
>     outputText <- return $ run programText
>     putStrLn outputText

Static Analyzer
---------------

The PL-{GOTO} language doesn't require any static analysis itself, but in
order to generate MSIL code from it, it's very useful to do some.

Label every loop used in the program with a unique ID.

> labelLoops (Block list) id =
>     let
>         (list', id') = labelList list id
>     in
>         (Block list', id')
> labelLoops (Loop _ n i) id =
>     let
>         (i', id') = labelLoops i id
>     in
>         (Loop id' n i', id'+1)
> labelLoops other id = (other, id)

> labelList [] id = ([], id)
> labelList (x:xs) id =
>     let
>         (x', id') = labelLoops x id
>         (xs', id'') = labelList xs id'
>     in
>         ((x':xs'), id'')

Helper function for the test suite.

> testLoopLabeling s = case parse program "" s of
>     Left perr -> show perr
>     Right prog -> show (labelLoops prog 0)

Gather all variables used in the program.  This includes internal variables
to be used as loop counters.  This assumes loops have already been labeled.

> gatherVars env (Block []) = env
> gatherVars env (Block (i:rest)) =
>     let
>         env' = gatherVars env i
>     in
>         gatherVars env' (Block rest)
> gatherVars env (Loop id n i) =
>     let
>         env' = register env n
>         env'' = gatherVars env' i
>     in
>         register env'' ("_loop" ++ (show id))
> gatherVars env (AssignZero n) =
>     register env n
> gatherVars env (AssignOther n m) =
>     register (register env n) m
> gatherVars env (AssignIncr n m) =
>     register (register env n) m

Compiler
--------

The compiler takes an abstract representation of a PL-{GOTO}
program and produces a string containing an MSIL program
(suitable as input to `ilasm`) which computes the same function,
modulo limitations like 32-bit integers.

The generated source consists of:

* a prelude, which is the same for all programs;
* a declaration of all the variables used in the program;
* generated code corresponding to the computation proper;
* a set of statements to display the final values of all variables; and
* a postlude, which is the same for all programs.

> translate ast =
>     let
>         (ast', _) = labelLoops ast 0
>         env = gatherVars empty ast'
>         varsBlock = makeVarsBlock env
>         codeBlock = genCode env ast'
>         dumpBlock = makeDumpBlock env
>     in
>         prelude ++ varsBlock ++ codeBlock ++ dumpBlock ++ postlude

We never push more than 32 values onto the stack at any given time
(actually, it's a lot less than that,) so we don't worry about computing
the maximum stack depth here.

> prelude  = ".assembly PLexceptGOTOprogram {}\n\
>            \.method static public void main() il managed\n\
>            \{\n\
>            \  .entrypoint\n\
>            \  .maxstack 32 // a guess\n"
> 
> postlude = "  ret\n\
>            \}\n"

> makeVarsBlock env =
>     let
>         localVarsBlock = makeLocalVarsBlock $ toList env
>     in
>         "  .locals init (" ++ localVarsBlock ++ ")\n"

> makeLocalVarsBlock [] = ""
> makeLocalVarsBlock [(key, value)] =
>     formatLocal key value
> makeLocalVarsBlock ((key, value):rest) =
>     (formatLocal key value) ++ ", " ++ makeLocalVarsBlock rest

> formatLocal key value = "int32 " ++ key

> makeDumpBlock env =
>     let
>         dumps = map (formatDump) (toList env)
>     in
>         foldl (++) "" dumps

> formatDump (name, pos)
>     | take 5 name == "_loop" = ""
>     | otherwise        = "  ldstr \"" ++ name ++ "=\"\n\
>                          \  call void [mscorlib]System.Console::Write(string)\n\
>                          \  ldloca.s " ++ name ++ "\n\
>                          \  call instance string [mscorlib]System.Int32::ToString()\n\
>                          \  call void [mscorlib]System.Console::WriteLine(string)\n"

Generate code for the given AST.  This is the meat of the compiler.

> genCode env (Block []) = ""
> genCode env (Block (i:rest)) =
>    (genCode env i) ++ (genCode env $ Block rest)
> genCode env (Loop id n i) =
>    let
>        loopName = "_loop" ++ (show id)
>        loopLabel = "LOOP" ++ (show id)
>        loopBody = genCode env i
>    in
>        "  // ---------- BEGIN " ++ loopLabel ++ "\n\
>        \  ldloc " ++ n ++ "\n\
>        \  stloc " ++ loopName ++ "\n\
>        \  br.s " ++ loopLabel ++ "_CHECK\n\
>        \  " ++ loopLabel ++ "_TOP:\n" ++
>        loopBody ++
>        "  // decrement " ++ loopName ++ "\n\
>        \  ldloc " ++ loopName ++ "\n\
>        \  ldc.i4.1\n\
>        \  sub\n\
>        \  stloc " ++ loopName ++ "\n\
>        \  " ++ loopLabel ++ "_CHECK:\n\
>        \  ldloc " ++ loopName ++ "\n\
>        \  ldc.i4.0\n\
>        \  bgt.s " ++ loopLabel ++ "_TOP\n\
>        \  // ---------- END " ++ loopLabel ++ "\n"
> genCode env (AssignZero n) =
>    "  ldc.i4.0\n\
>    \  stloc " ++ n ++ "\n"
> genCode env (AssignOther n m) =
>    "  ldloc " ++ m ++ "\n\
>    \  stloc " ++ n ++ "\n"
> genCode env (AssignIncr n m) =
>    "  ldloc " ++ m ++ "\n\
>    \  ldc.i4.1\n\
>    \  add\n\
>    \  stloc " ++ n ++ "\n"

Driver functions for compiler.

> compile s = case parse program "" s of
>     Left perr -> show perr
>     Right prog -> translate prog

> compileFile fileName = do
>     programText <- readFile fileName
>     outputText <- return $ compile programText
>     putStrLn outputText
