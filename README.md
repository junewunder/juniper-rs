# Juniper

This is a toy programming language that I plan on using to add fun features to and generally practice my pl skills.

An interesting part of this repository is a mixfix parser (located in `src/mixfix/mixfix.rs`). [The algorithm was copied from here.](https://github.com/uvm-plaid/uvmhs/blob/master/src/UVMHS/Lib/Parser/Mixfix.hs)

## Object branch goals:

 - [ ] enums
 - [ ] named top-level structs
 - [ ] anonymous expression objects
 - [ ] optional typing

## features so far:

 - bools, numbers, strings
 - mutability
 - while loops
 - primitives
 - error reporting on interpreting

## features to do:

 - arrays/objects/structs
 - match/case analysis
 - error reporting on parsing
 - top level constants
 - REPL functionality
 - custom parser error type
 - parse values from command line to main function
 - detect when mut value is trying to be used without being dereferenced
 - use Weak reference counting in CloV
 - strange out-of-bounds error when interp error is found on last line of file
 - mutable struct values
