# building v 8.1.14
```
git clone https://github.com/SWI-Prolog/swipl-devel.git
cd swipl-devel
git checkout V8.1.14
git submodule update --init
```
install build dependencies: https://www.swi-prolog.org/build/Debian.html

that is:
```
sudo apt-get install \
        build-essential cmake ninja-build pkg-config \
        ncurses-dev libreadline-dev libedit-dev \
        libunwind-dev \
        libgmp-dev \
        libssl-dev \
        unixodbc-dev \
        zlib1g-dev libarchive-dev \
        libossp-uuid-dev \
        libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
        libxpm-dev libxt-dev \
        libdb-dev \
        libpcre3-dev \
        libyaml-dev
```

build: https://github.com/SWI-Prolog/swipl-devel/blob/master/CMAKE.md

that is:
```
cd swipl-devel
mkdir build
cd build
cmake -G Ninja ..
ninja
ctest -j 8 --output-on-failure
ninja install
```
really do run those tests.



# pain points
### overlooking syntax errors. swipl keeps going. strange errors happen.

this should be mitigated by dev_runner.pl. If you have a usecase that it doesn't work for, let me know.

OT, if i make a script out of my current command line, 
```
#!/usr/bin/env bash

reset;echo -e "\e[3J";   swipl -s ../lib/dev_runner.pl   --problem_lines_whitelist=problem_lines_whitelist  ../tests/endpoint_tests/endpoint_tests2.pl  "set_flag(overwrite_response_files, false), set_flag(add_missing_response_files, false), run_tests"
```
i want to have a system worked out for exposing some parts of the command line as arguments. 

### findall vs maplist, fails: 
forgetting that a predicate is called from a findall and indicating error with a failure
    findall doesn't seem to be a good choice ever, as it's easy to hide unintended failures with it,
    and these happen a lot. could be mitigated by rdet, but, even if i'm able to fix the macro unexpansion so that it doesnt mess up variable names in gtrace,
it's still a severy DRY violation to try to keep rdet() declarations in sync with the code. possible solution:
    before importing rdet, import a macro that expands something like r(head :- body). But it wouldnt surprise me if this messed up var names again.
    Another reason to stay away from findall is doc.

### functional notation:
https://github.com/jarble/functional-prolog/blob/master/functional_prolog.pl (?)
https://github.com/mndrix/func/issues/12 (?)

https://github.com/awto/fnotation
	confuses gtrace? with so many bugs, it would be better if gtrace showed the generated code, like it happens with my dict macro, but it doesnt
	puts statements in the wrong place: 
		doesnt play with ( -> ; ), 
		doesnt play with yall
		doesnt play with dicts?
		doesnt play with =
	it's still the best one i found, but use it with care


https://www.j-paine.org/dobbs/grips.html (proprietary)

### issues
https://stackoverflow.com/questions/58758471/swi-prolog-yall-conflict-with-dicts


### things to study to improve current codebase

https://www.swi-prolog.org/pldoc/man?section=printmsg

https://www.swi-prolog.org/pldoc/man?predicate=debug/3

https://www.swi-prolog.org/pldoc/man?section=http-debug

https://swi-prolog.discourse.group/t/trigger-complete-reload-and-recompile/1011

https://rlaanemets.com/post/show/reporting-exception-stack-traces-in-a-swi-prolog-application

https://www.google.com/search?q=prolog+declarative+debugging


# in_case_we_are_stuck_with_pure_swipl

https://arxiv.org/pdf/0911.2899.pdf

https://arxiv.org/pdf/1909.08230.pdf

https://github.com/wysiib/plspec
https://arxiv.org/pdf/1909.08230.pdf

https://github.com/TeamSPoon/must_trace

https://github.com/logicmoo/fluentvars

https://github.com/jarble/imperative_prolog

https://github.com/edisonm/refactor - currently only implements term search and replace

https://github.com/CapelliC/prolog-snippets/blob/master/lifter.pl

https://github.com/tef/nomads

http://awarth.blogspot.com/2008/08/asserts-and-retracts-with-automatic.html

i wonder if an expected, unsurprising semantic would be a language feature / macro that would detect a use of something like Dict.put built with setarg within a rule, and automatically insert code that would essentially cause pass-by-value semantics when calling that rule.
ie if youre destructively changing a dict, you most likely dont want to propagate that change to the caller

https://perso.liris.cnrs.fr/emmanuel.coquery/tclp/doc.html

https://github.com/TeamSPoon/gvar_syntax

https://github.com/TeamSPoon/multimodal_dcg/blob/master/prolog/multimodal_dcg.pl

# profiling

http://www.swi-prolog.org/pldoc/doc_for?object=profile/2


# atoms vs strings

As a general rule, use atoms for internal identifiers and use strings if the data is to be used in I/O. Consider the following:

`
hello :- 
  writeln("Hello, world!").
`

(Note that the double-quotes indicate that "Hello, world!" is a string)

You could change all occurrences of the `hello` predicate to some other unique identifier like `foo` and the semantics of the program would not change: when you call `foo` the program will still output `Hello, world!`. On the other hand, consider:

`
hello :- 
  writeln('Hello, world!').
`

(Note that the single-quotes indicate that 'Hello, world!' is an atom)

You cannot substitute 'Hello, world!' with some other unique identifier, like `foo`. This will change the program's semantics as the program would then output `foo` instead of `Hello, world!`

Confer the SWI-PL documentation on this issue here: https://www.swi-prolog.org/pldoc/man?section=ext-dquotes-motivation

> ... Representing text using atoms is often considered inadequate for several reasons:
It hides the conceptual difference between text and program symbols. Where content of text often matters because it is used in I/O, program symbols are merely identifiers that match with the same symbol elsewhere. Program symbols can often be consistently replaced, for example to obfuscate or compact a program. ...
<<<<<<< Updated upstream

##..
``` up needing flatten/2 often   indicates, like append/3 for
%   appending two lists, a bad design. Efficient code that generates
%   lists from generated small  lists   must  use  difference lists,
%   often possible through grammar rules for optimal readability.
%
```

# pecularities of swipl plunit
variables arent preserved during the macro expansion phase as you would expect? The fact that plunit is based on macro expansion makes it possibe that your tests are silently ignored because of expansion errors / conflicts wiith other libraries..
```
:- begin_tests(x).

test(0, all((X=Y))) :-
    X = 5,
    Y = 6.

:- end_tests(x).

% PL-Unit: x . done
% test passed
``` 

rules with wrong arity are silently ignored:
``` 
:- begin_tests(x).
    
test(0, forall(x(X)), all((X=X))) :-
    fail.

:- end_tests(x).

% PL-Unit: x  done
% No tests to run
```


# parsing prolog code
	https://github.com/fnogatz/plammar
		almost works but not quite
	github.com:JanWielemaker/reindent
		just tokenizes and then does some smartness to fix indentation, not useful
	github.com:SWI-Prolog/packages-indent
		seems to be the way to go. queries prolog_read_source_term, then we can pattern-match the clause terms..
		```
		?- [A|B] =.. X.
		X = ['[|]', A, B].
		% so this actually works 

		```
		```
			comments suck in general. It hurts to accept it, but if you think about it, how could a parser ever read you mind to figure out what commment is associated to what part of your code? Comments on their own lines, comments at line ends, comments in arg lists, commented out code, crap crap crap. So, my idea is, comment with structure, right in prolog code. Add :- comment(predicate, blablabla), put nop(blabla) inside rule bodies, wrap calls in with_comment(....), whatever it takes.. 
		```
		hackery2/data/swipl/parse_prolog.pl


# SICSTUS
bundled clpfd is constrained to "[-2^60,2^60-1] on 64-bit platforms". This means we wouldn't be able to emulate rationals.
^ err not anymore?
how far along is https://github.com/triska/clpz ?
https://sicstus.sics.se/sicstus/docs/latest4/html/sicstus.html/FDBG-Introduction.html

