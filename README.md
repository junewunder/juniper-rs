# Juniper

This is a toy programming language that I plan on using to add fun features to and generally practice my pl skills.

An interesting part of this repository is a mixfix parser (located in `src/mixfix/mixfix.rs`). [The algorithm was copied from here.](https://github.com/uvm-plaid/uvmhs/blob/master/src/UVMHS/Lib/Parser/Mixfix.hs)

## questions
 - how to do implement no-arg functions like `random`
 - how to typecheck with mutual recursion??
 - .....how bad would it be to implement algorithm W....

## features so far:

 - bools, numbers, strings
 - mutability
 - while loops
 - primitives
 - error reporting on interpreting
 - arrays/objects/structs
 - match/case analysis

## features to do:

 - error reporting on parsing
 - modules
 - top level constants -> create BlockV which is closure but without arguments
 - REPL functionality
 - parse values from command line to main function
 - detect when mut value is trying to be used without being dereferenced
 - use Weak reference counting in CloV?
 - mutable struct values
 - bug: variable names that conatain keywords can't be parsed
 - scoped types?
