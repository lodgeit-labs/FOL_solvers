%:- module(_, [env_bool/2, flag/2]).

:- multifile user:flag_default/2.


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



 flag(Key, Value) :-
 	format(user_error,'flag(~q~n',[Key]),
	(	current_prolog_flag(Key, Value0)
	->	true
 	;	(	getenv(Key, Value0)
 		->	true
 		;	(	flag_default(Key, Value0)
			->	true
			;	throw_string(Key)))),
 	Value = Value0.

