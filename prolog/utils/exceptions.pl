
/*
	throw a msg(Message) term, these errors are caught by our http server code and turned into nice error messages
*/

throw_string_with_html(List_Or_Atomic, Html) :-
	prepare_throw(List_Or_Atomic, String),
	throw(error(msg(with_html(String, Html)),_)).

prepare_throw(List_Or_Atomic, String) :-
 	/* and then this could be removd in favor of the repl loop gtrace..*/
	(
		(
			flatten([List_Or_Atomic], List),
			maplist(stringize, List, List2),
			atomic_list_concat(List2, String)
		)
		->	true
		;	throw(internal_error)
	),
	(	current_prolog_flag(debug, true)
	->	(
			format(user_error, 'debug is true..'),
			gtrace_if_have_display
		)
	;	true).

 throw_string(List_Or_Atomic) :-
 	prepare_throw(List_Or_Atomic, String),
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


