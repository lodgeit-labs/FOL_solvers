/*
term(true,
	["true"]).
term('builtins#conj',
	[A,",",B]).
*/

genuri(Base, Uri) :-
	gensym(Base, Uri).


trc(X) :-
	open('trace1.js', write, Trace_file),
	write(Trace_file, 'import {f} from "./trace_import.js";\n'),
	b_setval(trace_file, Trace_file),
	node(X, "<dummy>", 'control:tracer_invocation', [Slot0], _),
	trc(Slot0, user, X, GP),
	print_term(GP, [write_options([
				numbervars(true),
				quoted(true),
				portray(true)])]),
	nl,
	close(Trace_file).


node(Query, Parent, Type, Args, Node) :-
	/* name the node */
	genuri(node, Node),
	/* name the slots */
	maplist(genuri(arg), Args),
	/*maplist(gen_slot, Args),*/
	/* write it out */
	term_string(Query,Query_str),
	gen(node{
		'@id': Node,
		parent: Parent,
		type: Type,
		args: Args,
		str: Query_str
	}).
/*
gen_slot(Arg) :-
	gen(node{
		'@id': Arg,
		type: 'delogic:slot'
	}).
*/
gen(Dict) :-
	b_getval(trace_file, Trace_file),
	write(Trace_file, 'f('),
	json_write(Trace_file, Dict, [serialize_unknown(true)/*, tag(type)*/]),
	write(Trace_file, ');\n'),
	flush_output(Trace_file).


trc(Parent,_,true,true) :-
	!,
	node(true,Parent, 'proof:true', 0, _, _).

trc(Parent,Module, (A,B),and(ProofA,ProofB)):-
	!,
	node((A,B), Parent, 'proof:conjunction', [Slot0,Slot1], _),
	trc(Slot0 ,Module, A, ProofA),
	trc(Slot1, Module, B, ProofB).

trc(Parent,Module, (A->B;C),ifthenelse(ProofA,ProofB,ProofC)):-
	!,
	node((A->B;C), Parent, 'proof:ifthenelse', [Slot0,Slot1,Slot2], _),
	(
		trc(Slot0,Module, A,ProofA)
		->
		trc(Slot1,Module, B,ProofB)
		;
		trc(Slot2,Module, C,ProofC)
	).

trc(Parent,Module, (A;B),or(Proof)):-
	A \= (_->_),
	!,
	node((A;B), Parent, 'proof:disjunction', [Slot0,Slot1], _),
	(
		trc(Slot0,Module, A,Proof)
	;
		trc(Slot1,Module, B,Proof)
	).

trc(Parent,Module, (A->B),ifthen(ProofA,ProofB)):-
	!,
	node((A->B), Parent, 'proof:ifthen', [Slot0,Slot1], _),
	(
		trc(Slot0,Module, A,ProofA)
	->
		trc(Slot1,Module, B,ProofB)
	).

trc(Parent,Module, ('\\+'(A)),not(A)):-
	!,
	writeq('\\+'(A)),nl,
	node('\\+'(A),Parent, 'proof:not', [Slot0], _),
	\+trc(Slot0,Module, (A), _).

trc(Parent,Module, (not(A)),not(A)):-
	!,
	writeq('not'(A)),nl,
	node(not(A), Parent, 'proof:not', [Slot0], _),
	\+trc(Slot0,Module, A, _).

trc(Parent,Module, A, (A :- Proof)) :-
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
	node(A, Parent, 'proof:call', [Slot0], _),
	trc(Slot0,Module, C, Proof).

trc(Parent,Module, catch(X,Y,Z), catch(Px,Y,Pz)) :-
	!,
	node(catch(X,Y,Z), Parent, 'proof:catch', [Sx,Sy,Sz], _),
	catch(
		trc(Sx, Module, X, Px),
		Y,
		(
			term_string(Sy, Sy_str),
			gen(arg{'@id': Sy, str: Sy_str}),
			trc(Sz, Module, Z, Pz)
		)
	).

trc(Parent,Module, Q, (Q :- Proof)):-
	functor(Q, Name, _),
	(	sub_atom(Name, 0, _, _, $)
	->	Body = builtin(Q)
	;	trc_clause(Module,Q,Body)),
	(	Body = loaded(X,Module2)
	->	(
			node(Q, Parent, 'proof:loaded', [Slot0], _),
			trc(Slot0, Module2, X, Proof)
		)
		;
		(
			Body = builtin(_),
			(	sub_atom(Name, 0, _, _, $)
			->	system_something_with_module(Module, Q, Q2)
			;	Q2 = Q),
			node(Q2, Parent, 'proof#builtin', [], _),
			call(Q2),
			Proof=builtin
		)
	).


/*
now Q is something like '$sig_atomic'(setup_trap_assertions(_32202))
what i'm trying to do is change that into '$sig_atomic'(module:setup_trap_assertions(_32202))
but it's useless
*/

system_something_with_module(Module, Q, Q2) :-
	(	Q =.. [_]
	->	Q2 = Module:Q
	;	(
			Q =.. [SysFn|R],
			(	R = [Fn]
			->	true
			;	throw_string('uhh too high arity rn')),
			Q2 =.. [SysFn,(Module:Fn)]
		)
	).

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

