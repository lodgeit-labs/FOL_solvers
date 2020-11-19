
/*
	throw a msg(Message) term, these errors are caught by our http server code and turned into nice error messages
*/
 throw_string(List_Or_Atom) :-
	gtrace_if_have_display,
	flatten([List_Or_Atom], List),
	maplist(stringize, List, List2),
	atomic_list_concat(List2, String),
	throw(error(msg(String),_)).


 have_display :-
	getenv('DISPLAY', Display),
	atom_length(Display, X),
	X > 0.

 gtrace_if_have_display :-
	(	have_display
		-> gtrace
		; true).

stringize(X, X) :-
	atomic(X).
stringize(X, Y) :-
	\+atomic(X),
	term_string(X, Y).
