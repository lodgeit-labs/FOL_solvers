
:- debug(checklist).

 print_debugging_checklist :-
	!print_debugging_checklist2.

 print_debugging_checklist2 :-

	%gtrace,
	debugging,

	findall(Topic, debugging(Topic), Topics),
	debug(checklist, 'topics: ~q', [Topics]),

	debug(checklist, 'misc_check on', []),

	env_bool('DETERMINANCY_CHECKER__USE__ENFORCER', DETERMINANCY_CHECKER__USE__ENFORCER),
	debug(checklist, 'DETERMINANCY_CHECKER__USE__ENFORCER ~q', [DETERMINANCY_CHECKER__USE__ENFORCER]),

	getenv('DISPLAY', DISPLAY),
	debug(checklist, 'DISPLAY ~q', [DISPLAY]),

	env_bool('ROBUST_ROL_ENABLE_CHECKS', ROBUST_ROL_ENABLE_CHECKS),
	debug(checklist, 'ROBUST_ROL_ENABLE_CHECKS ~q', [ROBUST_ROL_ENABLE_CHECKS]),

	env_bool('ENABLE_CONTEXT_TRACE_TRAIL', ENABLE_CONTEXT_TRACE_TRAIL),
	debug(checklist, 'ENABLE_CONTEXT_TRACE_TRAIL ~q', [ENABLE_CONTEXT_TRACE_TRAIL]),

	(doc_dump_server_is_inited(Params) -> debug(checklist, 'doc_dump_server_is_inited(~q)', [Params]) ; debug(checklist, 'doc_dump_server off', [])),

	flag(die_on_error, Die_on_error),
	debug(checklist, 'die_on_error ~q', [Die_on_error]),

	flag(gtrace, Gtrace),
	debug(checklist, 'gtrace ~q', [Gtrace]),

	env_bool('ROBUST_ENABLE_NICETY_REPORTS', ROBUST_ENABLE_NICETY_REPORTS),
	debug(checklist, 'ROBUST_ENABLE_NICETY_REPORTS ~q', [ROBUST_ENABLE_NICETY_REPORTS]),

	flush.

