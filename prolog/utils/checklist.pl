
 print_debugging_checklist :-
	!print_debugging_checklist2.

 print_debugging_checklist2 :-
	findall(_,
	(

		debugging,

		flag('DISABLE_GRACEFUL_RESUME_ON_UNEXPECTED_ERROR', Disable_graceful_resume_on_unexpected_error),
		format(user_error, '* 'DISABLE_GRACEFUL_RESUME_ON_UNEXPECTED_ERROR' ~q~n', [Disable_graceful_resume_on_unexpected_error]),

		flag('GTRACE_ON_OWN_EXCEPTIONS', Gtrace),
		format(user_error, '* GTRACE_ON_OWN_EXCEPTIONS ~q~n', [Gtrace]),

		(	getenv('DISPLAY', DISPLAY)
		->	true
		;	DISPLAY = '(none)'),
		format(user_error, '* DISPLAY ~q~n', [DISPLAY]),

		findall(Topic, debugging(Topic), Topics),
		format(user_error, '* topics: ~q~n', [Topics]),

		format(user_error, '* misc_check on~n', []),

		env_bool('DETERMINANCY_CHECKER__USE__ENFORCER', DETERMINANCY_CHECKER__USE__ENFORCER),
		format(user_error, '* DETERMINANCY_CHECKER__USE__ENFORCER ~q~n', [DETERMINANCY_CHECKER__USE__ENFORCER]),

		env_bool('ROBUST_ROL_ENABLE_CHECKS', ROBUST_ROL_ENABLE_CHECKS),
		format(user_error, '* ROBUST_ROL_ENABLE_CHECKS ~q~n', [ROBUST_ROL_ENABLE_CHECKS]),

		env_bool('ENABLE_CONTEXT_TRACE_TRAIL', ENABLE_CONTEXT_TRACE_TRAIL),
		format(user_error, '* ENABLE_CONTEXT_TRACE_TRAIL ~q~n', [ENABLE_CONTEXT_TRACE_TRAIL]),

		env_bool('ROBUST_ENABLE_NICETY_REPORTS', ROBUST_ENABLE_NICETY_REPORTS),
		format(user_error, '* ROBUST_ENABLE_NICETY_REPORTS ~q~n', [ROBUST_ENABLE_NICETY_REPORTS]),

		(
			(doc_dump_server_is_inited(Params))
		->	true
		;	Params = 'off'
		),
		format(user_error, '* doc_dump_server(~q)~n', [Params]),

		flush
	),
	_).


