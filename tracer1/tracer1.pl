/* i guess i should make this two-level:
first level pattern-matches on the code, can use cuts, and dispatches to the second level*/
trc(_,true,true).
trc(Module, (A,B),and(ProofA,ProofB)):-
	trc(Module, A,ProofA),trc(Module, B,ProofB).
trc(Module, (A->B;C),ifthenelse(ProofA,ProofB,ProofC)):-
	trc(Module, A,ProofA)
	->
	trc(Module, B,ProofB)
	;
	trc(Module, C,ProofC).
trc(Module, (A;B),or(Proof)):-
	A \= (_->_),
	(trc(Module, A,Proof);
	trc(Module, B,Proof)).
trc(Module, (A->B),ifthen(ProofA,ProofB)):-
	trc(Module, A,ProofA)
	->
	trc(Module, B,ProofB).

trc(Module, A, (A :- Proof)) :-
	catch(A =.. [call,Arg|Args],_,fail),
	(	atom(Arg)
	->	C =.. [Arg|Args]
	;	(
			Arg =.. [Fn|Args2],
			append(Args2,Args,New_args),
			C =.. [Fn|New_args]
		)
	),
	trc(Module, C, Proof).

trc(Module, Q, (Q :- Proof)):-
	writeq(Module-Q),nl,
	(	atom(Q)
	->	Name = Q
	;	compound_name_arity(Q, Name, _)),
	Name \= ',',Name \= ';',Name \= '->',Name \= 'call',

	(	sub_atom(Name, 0, _, _, $)
	->	Body = builtin(Q)
	;	trc_clause(Module,Q,Body)),

	writeq([Body]),nl,

	(
		(
			Body = builtin(X),
			%writeq(call(Module:X)),nl,
			(	sub_atom(Name, 0, _, _, $)
			->	call_system_something_with_module(Module, Q)
			;	call(Module:X)),
			Proof=builtin
		)
		;
		(
			Body = loaded(X,Module2),
			%writeq(Module2:X),nl,
			trc(Module2, X, Proof)
		)
	).

call_system_something_with_module(Module, Q) :-
	(	Q =.. [_]
	->	Q2 = Q
	;	(
			Q =.. [SysFn|R],
			(	R = [Fn]
			->	true
			;	throw_string('uhh')),
			Q2 =.. [SysFn,(Module:Fn)]
		)
	),
	call(Q2).


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
	trc(user, X, GP),
	print_term(GP, [write_options([
				numbervars(true),
				quoted(true),
				portray(true)])]),
	nl.
	%writeq(GP).



/*
debug,use_module(library(plunit)),gtrace,trc(run_tests).
*/
