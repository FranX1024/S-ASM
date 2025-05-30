# S-ASM Compiler

## Introduction

S-ASM is Lisp-like language of my own invention, intended to be elegant, powerful and simple to implement.
This repository includes 2 files; compiler.lisp, which, given the path of S-ASM source, compiles it to Motorola 68k assembly code that can be emulated using EASy68k and igrica.lisp, which contains the source for a hockey video game written in S-ASM.

Despite the Lispy syntax of S-ASM, it still lacks many key features that define Lisp, including:
 * Type system
 * Garbage collection
 * Lambda functions

In fact, S-ASM stands for S-expression ASsemBly.

## How it works

S-ASM compiles every function individually by traversing its syntax tree in post-order and compiling each operator into a predefined code snippet. All intermediate values are stored on stack.

S-ASM values are always collections of 4 bytes which, depending on the programmer's intention, can represent an integer, a truth value or a pointer.

Every form in S-ASM puts a 4-byte value on stack, or in other words, every form in S-ASM has a return value.

String literals and arrays are allocated statically. This is done by keeping track of declared strings and arrays during parsing and assigning them unique labels which are then used at the end of the assembly code to occupy needed memory.

## How do I learn its syntax?

Since this is a small project that I will likely not revisit without completely rewriting the code (first things first, I'd want to port this to ARM so that it can run on Raspberry PI), I have not written a detailed documentation. However, almost all of the built in functions / macros in S-ASM are a subset of Common Lisp. The main exception is the trap function, which sends an interrupt and is normally used for IO.

Example:

```lisp
(trap (register-name value)
      (register-name value)
      ...
      output-register-name)
```
`output-register-name` is optional and really only makes sense when the IO action you're attempting to do involves a response which is stored in a register.
