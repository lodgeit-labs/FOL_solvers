:- module(_, [
	op(812,fx,!),
	op(812,fx,?),

	/* must have one solution */
	'!'/1,
	'!'/2,
	'!'/3,
	'!'/4,

	/* must have zero or one solution */
	'?'/1

	/* must have one or more solutions(to be done) */
	/*'+'/1*/
]).


:- meta_predicate '!'(0).
:- meta_predicate '!'(1, ?).
:- meta_predicate '!'(2, ?, ?).
:- meta_predicate '!'(3, ?, ?, ?).


:- dynamic determinancy_checker_thrower/1.


determinancy_checker_throw_error(E) :-
	user:determinancy_checker_thrower(T),!,
	call(T,E).

determinancy_checker_throw_error(E) :-
	throw(E).

env_bool_is_true(Env_var_value) :-
	downcase_atom(Env_var_value, V),
	member(V, [1, '1', 'true', 'yes', 'on']),!.

env_bool_true(Key) :-
	getenv(Key, Val),
	env_bool_is_true(Val).

:- if(env_bool_true('DETERMINANCY_CHECKER__USE__ENFORCER')).
:- [determinancy_enforcer].
:- else.
:- [determinancy_checker_det_v2].
:- [determinancy_checker_semidet_v1].
:- endif.



prolog:message(deterministic_call_found_a_second_solution(X)) --> [deterministic_call_found_a_second_solution(X)].
%prolog:message(deterministic_call_failed(X)) --> [deterministic_call_failed(X)].
prolog:message(E) --> {E = error(determinancy_checker(X),_), term_string(X, Str)}, [Str].



/*
todo:
:- maplist(!member, [1], _).

	% wrt the possibility of getting multiple results for the above query, this would be a good place to introduce something like scopes or configurations into determinancy checker. That is, '!' would mean the default/only scope, you control what exactly it does by prolog flags, but you could also have, let's say, !(db), that would have a separate debug level. Ie, i may want to trust my code to run in release mode, but still don't trust the db data - don't want to check that my code doesn't produce multiple results, but want to check that the database doesn't contain multiple records.

*/


