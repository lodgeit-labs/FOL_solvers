#!/usr/bin/env swipl


:- use_module(library(semweb/rdf11)).
:- [library(sparqlprog)].


:- rdf_register_prefix(rdf,
'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(rdfs,
'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(rdf2,
'https://rdf.lodgeit.net.au/rdf2/').


main :-
	sparql_endpoint( l, 'http://localhost:10035/repositories/repo/'),

	format(user_error, 'pyco3 starting...\n', []),
	Spec = [
			[opt(task), type(atom), longflags([task]),
			help('Task Pointer URI')]],
	opt_arguments(Spec, Opts, []),
	memberchk(task(Pointer), Opts),
	process_task_pointer(Pointer).
	
process_task_pointer(Pointer) :-
	(l ?? rdf(Pointer, rdf:value, Value)),
		% wrt the possibility of getting multiple results for the above query, this would be a good place to introduce something like scopes or configurations of the determinancy checker. That is, !! is the default/only scope, you control what exactly it does by prolog flags, but you could also have, let's say, !!(db), that would have a separate debug level. Ie, i may want to trust my code to run in release mode, but still won't trust the db data.
	(l ?? rdf(Pointer, rdf2:data_is_in_graph, Graph)),

	writeln('got triples:\n====='),
    findall((S,P,O),
        (
            (l ?? rdf(S, P, O, Graph)),
            writeq((S, P, O)),
            nl
        ),
    _Triples),
	writeln('====\n===='),
	%writeq(Triples),
	nl.

:- initialization(main).

