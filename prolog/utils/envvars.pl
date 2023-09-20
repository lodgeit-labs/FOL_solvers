%:- module(_, [env_bool/2, flag/2]).

%:- multifile user:flag_default/2.

:- multifile flag_default/2.


 env_bool(Key, Val) :-
	flag(Key, Input),
	!,
	env_bool_value_is(Input,Val).

 env_bool_value_is(Env_var_value, true) :-
	downcase_atom(Env_var_value, V),
	member(V, [1, '1', 'true', 'yes', 'on']),!.

 env_bool_value_is(Env_var_value, false) :-
	downcase_atom(Env_var_value, V),
	member(V, [0, '0', 'false', 'no', 'off']),!.


:- discontiguous flag_default/2.


/* this naming is unfortunate as swipl already has flag/3 */
 flag(Key, Value) :-
 	%format(user_error,'flag(~q~n',[Key]),
	(	current_prolog_flag(Key, Value0)
	->	true
 	;	(	getenv(Key, Value0)
 		->	true
 		;	(	flag_default(Key, Value0)
			->	true
			;	throw_string(Key)))),
 	report_flag_use(Key, Value0),
 	Value = Value0.

 report_flag_use(Key, Value) :-
	(	nb_current(flag_use, Flag_use)
	->	true
	;	Flag_use = []),

	(	member(Key, Flag_use)
	->	true
	;	(
			nb_setval(flag_use, [Key|Flag_use]),
			format(user_error,'flag(~q, ~q)~n',[Key, Value])
		)
	).

