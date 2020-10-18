:- module(_, [
	op(812,fx,!),
	op(812,fx,?),
	'!'/1,
	'!'/2,
	'!'/3,
	'?'/1
]).


:- meta_predicate '!'(0).
:- meta_predicate '!'(1, ?).
:- meta_predicate '!'(2, ?, ?).


:- if(getenv('DETERMINANCY_CHECKER__USE__ENFORCER', true)).
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

	% wrt the possibility of getting multiple results for the above query, this would be a good place to introduce something like scopes or configurations into determinancy checker. That is, '!' would mean the default/only scope, you control what exactly it does by prolog flags, but you could also have, let's say, !(db), that would have a separate debug level. Ie, i may want to trust my code to run in release mode, but still don't trust the db data.


*/


