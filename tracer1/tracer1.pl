/* i guess i should make this two-level:
first level pattern-matches on the code, can use cuts, and dispatches to the second level*/
trc(true,true).
trc((A,B),and(ProofA,ProofB)):-
	trc(A,ProofA),trc(B,ProofB).
trc((A->B;C),ifthenelse(ProofA,ProofB,ProofC)):-
	trc(A,ProofA)
	->
	trc(B,ProofB)
	;
	trc(C,ProofC).
trc((A;B),or(Proof)):-
	A \= (_->_),
	(trc(A,Proof);
	trc(B,Proof)).
trc((A->B),ifthen(ProofA,ProofB)):-
	trc(A,ProofA)
	->
	trc(B,ProofB).

trc(A, (A :- Proof)) :-
	A =.. [call,Arg|Args],
	(	atom(Arg)
	->	C =.. [Arg|Args]
	;	(
			Arg =.. [Fn|Args2],
			append(Args2,Args,New_args),
			C =.. [Fn|New_args]
		)
	),
	trc(C, Proof).

trc(Q, (Q :- Proof)):-
	compound_name_arity(Q, Name, _),
	Name \= ',',
	Name \= ';',
	Name \= '->',
	Name \= 'call',
	catch(
		(clause(Q,B),Body = user(B)),
		_,
		(Body = builtin(Q))
	),
	(	Body = builtin(X)
	->	(call(X),Proof=builtin)
	;	(Body = user(X), trc(X, Proof))).


trc(X) :-
	trc(X, GP),
	writeq(GP).

