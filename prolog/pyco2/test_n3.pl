#!/usr/bin/env swipl


:- use_module(n3).

x :-
	Spec = [
		[opt(n3file), type(atom), shortflags([n]), longflags([n3file]),
			help('the N3 file you wish to parse')]],
	opt_arguments(Spec, Opts, Args),
	(Args = [] -> true ; throw(string('no positional arguments accepted'))),
	memberchk(n3file(FilePath), Opts),
	parse_n3(FilePath, X),
	writeq(X).


:- initialization(x).
