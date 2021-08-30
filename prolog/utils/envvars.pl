:- module(_, [env_bool/2, flag/2]).

:- multifile user:env_bool_has_default/2.

:- multifile user:flag_default/2.


 env_bool(Key, true) :-
	getenv(Key, Val),
	!,
	env_bool_value_is_true(Val).

 env_bool(Key, false) :-
	getenv(Key, Val),
	!,
	env_bool_value_is_false(Val).

 env_bool(Key, Val) :-
	env_bool_has_default(Key, Val).

 env_bool_value_is_true(Env_var_value) :-
	downcase_atom(Env_var_value, V),
	member(V, [1, '1', 'true', 'yes', 'on']),!.

 env_bool_value_is_false(Env_var_value) :-
	downcase_atom(Env_var_value, V),
	member(V, [0, '0', 'false', 'no', 'off']),!.


 flag(Key, Value) :-
	(	current_prolog_flag(Key, Value)
 	->	true
 	;	flag_default(Key, Value)).
