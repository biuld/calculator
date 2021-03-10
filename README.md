# calculator

> an arithmetic calculator written with compiling theory in mind

- This repo is intended to be a follow-up practice after watching Mr. Landwerth's building a complier tutorial (see the link below) and  refresh my knowledge of compiling theory back in the school.
- This repo is not a line-to-line replica to Mr. Landwerth's Minsk Complier (written in C# with OO design).
- I try to write everything from scratch. (might change my mind later :-0 )
- I hope eventualy *calculator* can be a full fledged complier targeting WebAssembly. 

![demo](./img/demo.gif)

command list:

```
:context
:enableAST
:disableAST
:help
:quit
```

# Status

For now *calculator* supports:

- basic arithmetic computation with parentheses
- if expression
- assignments & variables
- type check

You can check out all the test cases under `test/Spec.hs`

# Reference

> [Exploring Languages with Interpreters and Functional Programming 2018 Version By H. Conrad Cunningham](https://john.cs.olemiss.edu/~hcc/csci450/ELIFP/)
> [Building a Compiler By Immo Landwerth](https://www.youtube.com/playlist?list=PLRAdsfhKI4OWNOSfS7EUu5GRAVmze1t2y)

