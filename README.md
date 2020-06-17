# Juniper

This is a toy programming language that I plan on using to add fun features to and generally practice my pl skills.

An interesting part of this repository is a mixfix parser (located in `src/mixfix/mixfix.rs`). [The algorithm was copied from here.](https://github.com/uvm-plaid/uvmhs/blob/master/src/UVMHS/Lib/Parser/Mixfix.hs)

## features so far:

 - bools, numbers, strings
 - mutability
 - while loops
 - primitives
 - error reporting on interpreting
 - arrays/objects/structs
 - match/case analysis

## features to do:

 - traits
 - modules
 - type inference
 - parser errors

 - REPL functionality
 - scoped types?
 - parse values from command line to main function
 - detect when mut value is trying to be used without being dereferenced
 - mutable struct values
 - bug: variable names that start with keywords can't be parsed

## plan for generics
add types GenericT(t, var) and ConcreteT(t, val)
assume there is a type: T x y z, which should be instantiated to T num string bool
 - x = num
 - y = string
 - z = bool
then the type AST will be
```
ConcreteT(
  ConcreteT(
    ConcreteT(
      GenericT(
        GenericT(
          GenericT(
              ...T...
            , "z")
          , "y")
        , "x")
      , num)
    , string)
  , bool)
```
