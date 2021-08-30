 grab_and_inc_current_num(K, V) :-
 	b_current_num_with_default(K,0,V),
 	Next is V + 1,
 	b_setval(K, Next).

 b_current(K,V) :-
	/* this really reads a backtracking value, but there's no b_current in stdlib */
	nb_current(K,V).

 b_current_num_with_default(K,Default,V) :-
	(
		catch(
			b_current_num(K,V),
			_,
			fail
		)
	->	true
	;	Default = V).

/* another swipl crapulooza workaround */
 b_current_num(K, V) :-
	(
		(
			b_current(K, V),
			V\=[]
		)
	->	true
	;	false).
