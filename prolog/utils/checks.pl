



/* This is like an assertion, but enabled/disabled separately. Always on for now, possibly to be disabled sometime in production. */

:- meta_predicate 'misc_check'(0).

 misc_check(Goal) :-
	call(Goal).





:- meta_predicate 'nicety'(0).

env_bool_has_default('ROBUST_ENABLE_NICETY_REPORTS', true).

:- if(env_bool('ROBUST_ENABLE_NICETY_REPORTS', false)).

	 nicety(_).

:- else.

	 nicety(Goal) :-
		call(Goal).

:- endif.




