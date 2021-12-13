# resources:
https://www.informatik.uni-kiel.de/~mh/FLP/implementations.html


## with some semblance of production-readiness:

### Visual Prolog:
fairly interesting language, seems to embrace imperativeness, has some sort of mutable variable class, but i'm not able to find a description of the exact semantics. No clp lib.


### ciao - ciao is hot
01.09.19 00:38:27<editable-log-koo>even compilation to javascript 
01.09.19 00:41:42<editable-log-koo>Backtrackable global variables
01.09.19 00:51:13<stoopkid> could be nice to experiment with
01.09.19 01:16:16<editable-log-koo>lotsa nice stuff
01.09.19 01:19:22<editable-log-koo>well, swipl's clpq is written all in prolog as well, so, maybe hacking
 it could be reasonable too
01.09.19 01:36:50<editable-log-koo>ciao's should prolly be a lot more hackable
the problem with ciao is that it's not mainstream. Just basic cli debugger. limited set of libraries. etc

### mercury, curry, oz, maybe something else: nice features wrt for examle clp and functional notation, but none really mature? 

mercury has a long way to go it seems: Solver types are an experimental addition to the language supporting the implementation of constraint solvers.
`A program may place constraints on and between variables of a solver type, limiting the values those variables may take on before they are actually bound. For example, if X and
Y are variables belonging to a constrained integer solver type, we might place constraints upon them such that X > 3 + Y and Y =< 7. `



### swipl
I have a couple of issues with swipl, or with using it the way we do, for what we do. 

Ecosystem: The fact that gtrace could use some more work is one. There seems to be only one equation solving library (PRESS), with dubious status. 

Clpq: will we be able to tread additional info / produce proofs?

My issues with the language itself all seem to be solvable by building a fairly thin reversible layer of a language that would compile to swipl prolog. 

Using compile-time macros just complicates debugging and confuses gtrace, all of this would be better done as a proper compilation step:

Global mutable state that is correctly retracted on backtracking. Automatic rdet-like functionality. dict_vars. Abstraction of recursion variables. Avoiding repetition caused by the module system.


### python

a full switch to python is also an option. Both me and bob have experience implementing logic programming in python, and gave some time to exploring integration of the paradigms, so if the project eventually started calling for logic programming, we'd probably figure out how to move forward. On top of that, we'd have sympy and other nice libraries. 


### Possibly we could also migrate to scala or clojure.

# tools
https://github.com/edisonm/refactor

