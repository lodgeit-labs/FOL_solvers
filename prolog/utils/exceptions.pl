/*
	throw a msg(Message) term, these errors are caught by our http server code and turned into nice error messages
*/


 throw_format(Format, Args) :-
 	length(Args,_),
 	assertion((atom(Format);string(Format))),
 	format(string(S), Format, Args),
	throw_string(S).


 throw_string(List_Or_Atomic) :-
 	throw_stringize_and_concat(List_Or_Atomic, String),
 	throw_value(String).


 throw_string_with_html(List_Or_Atomic, Html) :-
 	throw_stringize_and_concat(List_Or_Atomic, String),
	throw_value(with_html(String, Html)).


 throw_value(V) :-
 	(	current_prolog_flag(debug, true)	->	trace	;	true),
 	get_prolog_backtrace_str(Backtrace_str),
	throw(with_backtrace_str(error(msg(V),_),Backtrace_str)).


 throw_stringize_and_concat(List_Or_Atomic, String) :-
	(
		(
			flatten([List_Or_Atomic], List),
			maplist(stringize, List, List2),
			atomic_list_concat(List2, String)
		)
		->	true
		;	/* careful not to use throw_string here */
			throw(internal_error)
	).


 have_display :-
 	format(user_error, 'have_display?', []),
	getenv('DISPLAY', Display),
	atom_length(Display, X),
	X > 0,
	format(user_error, 'yes\n', []).


 gtrace_if_have_display :-
	(	have_display
	->	(
			(	\+current_prolog_flag(gtrace, false)
			)
		->	(
				format(user_error, '**********', []),
				backtrace(200),
				format(user_error, '**********', []),
				trace
			)
		;	true)
	; true).


 stringize(X, X) :-
	atomic(X).
 stringize(X, Y) :-
	\+atomic(X),
	term_string(X, Y).


 get_prolog_backtrace_str(Backtrace_str) :-
	get_prolog_backtrace(200, Backtrace, [goal_depth(7)]),
	stt(Backtrace, Backtrace_str).
