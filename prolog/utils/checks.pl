
/* This is like an assertion, but enabled/disabled separately. Always on for now, possibly to be disabled sometime in production. */

:- meta_predicate 'misc_check'(0).

misc_check(Goal) :-
	call(Goal).


:- debug(checklist).


 print_debugging_checklist :-

	%gtrace,
	debugging,
	findall(Topic, debugging(Topic), Topics),
	debug(checklist, 'topics: ~q', [Topics]),

	debug(checklist, 'misc_check on', []),

	env_bool('DETERMINANCY_CHECKER__USE__ENFORCER', DETERMINANCY_CHECKER__USE__ENFORCER),
	debug(checklist, 'DETERMINANCY_CHECKER__USE__ENFORCER ~q', [DETERMINANCY_CHECKER__USE__ENFORCER]),

	env_bool('DETERMINANCY_CHECKER__USE__UNDO', DETERMINANCY_CHECKER__USE__UNDO),
	debug(checklist, 'DETERMINANCY_CHECKER__USE__UNDO ~q', [DETERMINANCY_CHECKER__USE__UNDO]),

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






	flush.

