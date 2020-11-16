
/*
	throw a msg(Message) term, these errors are caught by our http server code and turned into nice error messages
*/
 throw_string(List_Or_Atom) :-
	flatten([List_Or_Atom], List),
	maplist(stringize, List, List2),
	atomic_list_concat(List2, String),
	gtrace_if_have_display,
	context_string(Ctx_str),
	atomics_to_string([String,'\n',Ctx_str], Str),
	throw(error(msg(Str),_)).

context_string(Str) :-
	get_context(C),
	(	C = []
	->	Str = ''
	;	(
			context_string1(1, C, Item_strings),
			atomics_to_string(['during:\n' | Item_strings], Str)
		)
	).

context_string1(Number, [C|Rest], [Str|Str_rest]) :-
	context_string2(Number, C, Str),
	Next is Number + 1,
	context_string1(Next, Rest, Str_rest).

context_string1(_, [],[]).

context_string2(Number, C, Str) :-
	(	atomic(C)
	->	atomics_to_string([Number, ') ', C, '\n'], Str)
	;	format(string(Str), '~q) ~q~n', [Number, C])).


gtrace_if_have_display :-
	(
		(
			getenv('DISPLAY', Display),
			atom_length(Display, X),
			X > 0
		)
		-> gtrace
		; true
	).

stringize(X, X) :-
	atomic(X).
stringize(X, Y) :-
	\+atomic(X),
	term_string(X, Y).

/*
	catch_with_backtrace doesnt exist on older swipl's
*/
catch_maybe_with_backtrace(A,B,C) :-
	(	current_predicate(catch_with_backtrace/3)
	->	catch_with_backtrace(A,B,C)
	;	catch(A,B,C)).

