/*
term(true,
	["true"]).
term('builtins#conj',
	[A,",",B]).
*/

node(Parent, Type, ArgCount, Args, Node) :-
	length(Args, ArgCount),
	maplist(genuri(arg), Args),
	gen(node{
		parent: Parent,
		functor: Type,
		args: Args
	}).


trc(Parent,_,true,true) :-
	!,
	node(Parent, 'builtins#true', 0, _, _).

trc(Parent,Module, (A,B),and(ProofA,ProofB)):-
	!,
	node(Parent, 'builtins#conjunction', 2, [Slot0,Slot1], _),
	trc(Slot0 ,Module, A, ProofA),trc(Slot1, Module, B, ProofB).

trc(Module, (A->B;C),ifthenelse(ProofA,ProofB,ProofC)):-
	!,
	node(Parent, 'builtins#ifthenelse', 3, [Slot0,Slot1,Slot2], _),
	(
		trc(Slot0,Module, A,ProofA)
		->
		trc(Slot1,Module, B,ProofB)
		;
		trc(Slot2,Module, C,ProofC)
	).

trc(Module, (A;B),or(Proof)):-
	A \= (_->_),
	!,
	node(Parent, 'builtins#disjunction', 2, [Slot0,Slot1], _),
	(
		trc(Slot0,Module, A,Proof)
	;
		trc(Slot1,Module, B,Proof)
	).

trc(Module, (A->B),ifthen(ProofA,ProofB)):-
	!,
	node(Parent, 'builtins#ifthen', 2, [Slot0,Slot1], _),
	(
		trc(Slot0,Module, A,ProofA)
	->
		trc(Slot1,Module, B,ProofB)
	).

trc(Module, ('\\+'(A)),not(A)):-
	!,
	writeq('\\+'(A)),nl,
	node(Parent, 'builtins#not', 1, [Slot0], _),
	\+trc(Slot0,Module, (A), _).

trc(Module, (not(A)),not(A)):-
	!,
	writeq('not'(A)),nl,
	node(Parent, 'builtins#not', 1, [Slot0], _),
	\+trc(Slot0,Module, A, _).

trc(Module, A, (A :- Proof)) :-
	functor(A, call, _),
	!,

	A =.. [call,Arg|Args],
	(	atom(Arg)
	->	C =.. [Arg|Args]
	;	(
			Arg =.. [Fn|Args2],
			append(Args2,Args,New_args),
			C =.. [Fn|New_args]
		)
	),
	node(Parent, 'builtins#call', 1, [Slot0], _),
	trc(Slot0,Module, C, Proof).

trc(Module, Q, (Q :- Proof)):-
	functor(Q, Name, _),
	(	sub_atom(Name, 0, _, _, $)
	->	Body = builtin(Q)
	;	trc_clause(Module,Q,Body)),
	(	Body = loaded(X,Module2)
	->	(
			node(Parent, 'builtins#call', 1, [Slot0], _),
			trc(Module2, X, Proof)
		)
		;
		(
			Body = builtin(X),
			(	sub_atom(Name, 0, _, _, $)
			->	call_system_something_with_module(Module, Q)
			;	(
					call(Module:X),
					gennode(Parent, exit, TraceNode),
					genprop(TraceNode, goal, X)
				)
			),
			Proof=builtin
		)
	).


/*
now Q is something like '$sig_atomic'(setup_trap_assertions(_32202))
what i'm trying to do is change that into '$sig_atomic'(module:setup_trap_assertions(_32202))
but it's useless
*/

call_system_something_with_module(Module, Q) :-
	(	Q =.. [_]
	->	Q2 = Q
	;	(
			Q =.. [SysFn|R],
			(	R = [Fn]
			->	true
			;	throw_string('uhh too high arity rn')),
			Q2 =.. [SysFn,(Module:Fn)]
		)
	),
	call(Q2).

/*
what you gotta do when looking up clauses is, i start with 'user' module, and you can't just naively call clause(Query, Body), you have to call clause(Module:Query, Body).
If it finds a builtin, it throws an error lol
if it finds a user clause, it succeeds
*/


trc_clause(Module,Q,Body) :-
	catch(
		clause(Module:Q,B,Ref),
		_,
		(Body = builtin(Q))
	),
	(	var(Body)
	->	(
			Body = loaded(B,Module2),
			(
				(
					clause_property(Ref, module(Module2)),
					Module2 \= system
				)
			->	true
			;	Module2=Module
			)
		)
	;	true).



trc(X) :-
	gennode(none, root, Parent),
	trc(Parent, user, X, GP),
	print_term(GP, [write_options([
				numbervars(true),
				quoted(true),
				portray(true)])]),
	nl.
	%writeq(GP).


gennode :-



/*
swipl -s trace_tests.pl -g 'use_module(library(plunit)),trc(run_tests)'

Warning: /home/koom/lodgeit2/master2/sources/depr/src/event_calculus.pl:163:
Warning:    Singleton variables: [P]
user-run_tests
[loaded((cleanup,setup_call_cleanup(setup_trap_assertions(_32202),run_current_units,report_and_cleanup(_32202))),plunit)]
plunit-cleanup
[loaded((thread_self(_32310),retractall(passed(_32314,_32316,_32318,_32320,_32322)),retractall(failed(_32330,_32332,_32334,_32336)),retractall(failed_assertion(_32344,_32346,_32348,_32350,_32352,_32354,_32356)),retractall(blocked(_32364,_32366,_32368,_32370)),retractall(sto(_32378,_32380,_32382,_32384)),retractall(fixme(_32392,_32394,_32396,_32398,_32400)),retractall(running(_32408,_32410,_32412,_32414,_32310))),plunit)]
plunit-thread_self(_32310)
[builtin(thread_self(_32310))]
plunit-retractall(passed(_32314,_32316,_32318,_32320,_32322))
[builtin(retractall(passed(_32314,_32316,_32318,_32320,_32322)))]
plunit-retractall(failed(_32330,_32332,_32334,_32336))
[builtin(retractall(failed(_32330,_32332,_32334,_32336)))]
plunit-retractall(failed_assertion(_32344,_32346,_32348,_32350,_32352,_32354,_32356))
[builtin(retractall(failed_assertion(_32344,_32346,_32348,_32350,_32352,_32354,_32356)))]
plunit-retractall(blocked(_32364,_32366,_32368,_32370))
[builtin(retractall(blocked(_32364,_32366,_32368,_32370)))]
plunit-retractall(sto(_32378,_32380,_32382,_32384))
[builtin(retractall(sto(_32378,_32380,_32382,_32384)))]
plunit-retractall(fixme(_32392,_32394,_32396,_32398,_32400))
[builtin(retractall(fixme(_32392,_32394,_32396,_32398,_32400)))]
plunit-retractall(running(_32408,_32410,_32412,_32414,main))
[builtin(retractall(running(_32408,_32410,_32412,_32414,main)))]
plunit-setup_call_cleanup(setup_trap_assertions(_32202),run_current_units,report_and_cleanup(_32202))
[loaded(setup_call_catcher_cleanup(setup_trap_assertions(_32202),run_current_units,_35784,report_and_cleanup(_32202)),plunit)]
plunit-setup_call_catcher_cleanup(setup_trap_assertions(_32202),run_current_units,_35784,report_and_cleanup(_32202))
[loaded(('$sig_atomic'(setup_trap_assertions(_32202)),'$call_cleanup'),plunit)]
plunit-'$sig_atomic'(setup_trap_assertions(_32202))
[builtin('$sig_atomic'(setup_trap_assertions(_32202)))]
plunit-'$call_cleanup'
[builtin('$call_cleanup')]
ERROR: -g use_module(library(plunit)),trc(run_tests): call_system_something_with_module/2: Unknown procedure: '$call_cleanup'/0
ERROR:   However, there are definitions for:
ERROR:         call_cleanup/2
ERROR:         call_cleanup/3
*/

