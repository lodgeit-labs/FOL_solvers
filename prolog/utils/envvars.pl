:- module(_, [env_bool_true/1, env_bool_false/1]).

 env_bool_true(Key) :-
	getenv(Key, Val),
	env_bool_value_is_true(Val).

 env_bool_value_is_true(Env_var_value) :-
	downcase_atom(Env_var_value, V),
	member(V, [1, '1', 'true', 'yes', 'on']),!.

 env_bool_false(Key) :-
	getenv(Key, Val),
	env_bool_value_is_false(Val).

 env_bool_value_is_false(Env_var_value) :-
	downcase_atom(Env_var_value, V),
	member(V, [0, '0', 'false', 'no', 'off']),!.

