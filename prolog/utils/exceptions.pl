
/*
	throw a msg(Message) term, these errors are caught by our http server code and turned into nice error messages
*/
 throw_string(List_Or_Atomic) :-
 	/* and then this could be removd in favor of the repl loop gtrace..*/
	gtrace_if_have_display,
	flatten([List_Or_Atomic], List),
	maplist(stringize, List, List2),
	atomic_list_concat(List2, String),
	throw(error(msg(String),_)).

 throw_format(Format, Args) :-
 	length(Args,_),
 	assertion(atom(Format)),
 	format(string(S), Format, Args),
	throw_string(S).

 have_display :-
 	format(user_error, 'have_display?', []),
	getenv('DISPLAY', Display),
	atom_length(Display, X),
	X > 0,
	format(user_error, 'yes', []).

 gtrace_if_have_display :-
	(	have_display
		-> gtrace
		; true).

 stringize(X, X) :-
	atomic(X).
 stringize(X, Y) :-
	\+atomic(X),
	term_string(X, Y).


