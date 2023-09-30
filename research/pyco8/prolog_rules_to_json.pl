:- multifile r/1.

:- consult('tests/pyco8_test1.pl').

%:- ['../utils/compile_with_variable_names_preserved.pl'].


try_get_variable_naming(Var, (Name = Var)) :-
	var_property(Var, name(Name)),
	!.
try_get_variable_naming(Var, ('_' = Var)).


user:term_expansion(P, x) :-
	P =.. [r|X],
	term_variables(P, Vars),
	maplist(try_get_variable_naming, Vars, Names),
	p_decl_to_json(P, Json),

	.

'make sure no rule declarations remained unexpanded' :-
	(
		(
				% see if there are any 'r' clauses to call. If there are not, we get an exception, catch it, all is well.
				catch(r(Bad),_,false),
				throw_string('p/1 declaration remained unexpanded'(Bad))
		);
		true
	).


:- initialization('make sure no rule declarations remained unexpanded').
