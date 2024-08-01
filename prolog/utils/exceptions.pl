
prolog:error_message(msg(Msg)) --> [Msg].
    
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


 flag_default('GTRACE_ON_OWN_EXCEPTIONS', true).

 throw_value(V) :-

	format(user_error, 'throw_value: ~q\n', [V]),
	(	env_bool('GTRACE_ON_OWN_EXCEPTIONS', true)
	->	gtrace_if_have_display
	;	true),

	format(user_error, 'get_prolog_backtrace_str\n', []),
	get_prolog_backtrace_str(Backtrace_str),
	format(user_error, 'context_string\n', []),
	context_string(Context_str),
	format(user_error, 'throw\n', []),

	(	tracing
	->	format(user_error, 'tracing.\n', [])
	;	format(user_error, 'not tracing.\n', [])),
	
	% swipl is somehow obsessed with connecting to X11:
	%2024-08-01T13:44:20Z app[1857704a33e118] mad [info]INFO:app.call_prolog:not tracing.
    %2024-08-01T13:44:21Z app[1857704a33e118] mad [info]INFO:app.call_prolog:% The graphical front-end will be used for subsequent tracing
    %2024-08-01T13:44:21Z app[1857704a33e118] mad [info]INFO:app.call_prolog:[PCE fatal: @display/display: Failed to connect to X-server at `': no DISPLAY environment variable
    %2024-08-01T13:44:21Z app[1857704a33e118] mad [info]INFO:app.call_prolog:*********************************************************************
	%2024-08-01T13:44:21Z app[1857704a33e118] mad [info]INFO:app.call_prolog:* You MUST be running the X11 Windowing environment.  If you are,   *
	% It only happens in the fly machine.	
	% it seems to go away on V9.3.8	
	%set_prolog_flag(debug_on_error, false),
	
	throw(with_processing_context(with_backtrace_str(error(msg(V),_),Backtrace_str),Context_str)).


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
	format(user_error, 'have_display?\n', []),
	(	(
			getenv('DISPLAY', Display),
			atom_length(Display, X),
			X > 0
		)
	->	format(user_error, 'have_display?yes\n', [])
	;	format(user_error, 'have_display?no\n', [])).


:- discontiguous flag_default/2.

 flag_default(gtrace, true).

 gtrace_if_have_display :-
 	format(user_error, 'gtrace_if_have_display\n', []),
	(	have_display
	->	(	env_bool('GTRACE_ON_OWN_EXCEPTIONS', true)
		->	(
				format(user_error, '*****vvvbacktrace?vvv*****', []),
				catch(backtrace(200),_,true),
				format(user_error, '*****^^^backtrace?^^^*****', []),
				trace,
				format(user_error, '*****.......*****', [])
			)
		;	true)
	; true).


 stringize(X, X) :-
	atomic(X).
 stringize(X, Y) :-
	\+atomic(X),
	term_string(X, Y).


 get_prolog_backtrace_str(Backtrace_str) :-
	catch(
		(
			get_prolog_backtrace(200, Backtrace, [goal_depth(7)]),
			stt(Backtrace, Backtrace_str)
		),
		_,
		Backtrace_str = 'get_prolog_backtrace threw exception.').



:- meta_predicate 'assertion2'(0).
 assertion2(Callable) :-
 	(	call(Callable)
 	->	true
 	;	throw_format('assertion failed: ~q', [Callable])).
