#!/usr/bin/env swipl


/*

WARNING: you must edit /usr/local/lib/swipl/library/semweb/sparql_client.pl (or wherever it is on your system) and make sure that application/sparql-results+json goes before xml. Otherwise, agraph sends back xml, and sparql_client.pl sparql_read_xml_result/2 has "space(remove)", which removes whitespace (including newlines) from literals. See also https://github.com/SWI-Prolog/packages-semweb/issues/91 .





*/


/*
:- use_module(library(fnotation)).
:- fnotation_ops($>,<$).
:- op(900,fx,<$).
*/


%:- ['../determinancy_checker/determinancy_checker_main.pl'].
:- [library(sparqlprog)].
:- use_module(n3).
:- ['../utils/utils.pl'].



:- rdf_register_prefix(rdf2,
'https://rdf.lodgeit.net.au/rdf2/').
:- rdf_register_prefix(tc,
'https://rdf.lodgeit.net.au/testcase/').



main :-
	debug(sparqlprog),
	sparql_endpoint( l, 'http://localhost:10035/repositories/repo/'),
	
	/*sparql_endpoint( l, 'https://integbio.jp/rdf/sparql'),
	(l ?? rdf(S, P, O, Graph)),
	halt,*/
	
	%?results-format=sparql-json
	format(user_error, 'pyco3 starting...\n', []),
	Spec = [
			[opt(task), type(atom), longflags([task]),
			help('Task Pointer URI')]],
	opt_arguments(Spec, Opts, []),
	memberchk(task(Pointer), Opts),
	process_task_pointer(Pointer).
	

process_task_pointer(Pointer) :-

	% Task has to be an iri, if it's a bnode, we have no way to find it among the triples obtained in fetch_triples.
	(l ?? rdf(Pointer, rdf:value, Task)),
		% wrt the possibility of getting multiple results for the above query, this would be a good place to introduce something like scopes or configurations of the determinancy checker. That is, !! is the default/only scope, you control what exactly it does by prolog flags, but you could also have, let's say, !!(db), that would have a separate debug level. Ie, i may want to trust my code to run in release mode, but still won't trust the db data.
	(l ?? rdf(Pointer, rdf2:data_is_in_graph, Graph)),
	fetch_triples(Graph),
	process_task(Task).


fetch_triples(Graph) :-
	writeln('got triples:\n====='),
	rdf_global_id(tc:text, P),
	findall((S,P,O),
		(
			%gtrace,
			
			
			
			/*
			sparql_read_xml_result(Input, Result) :-
    load_structure(Input, DOM,
                   [ dialect(xmlns),
                     space(remove)
                     ^^^ that's a problem, newlines are stripped.
*/
			
			
			(l ?? rdf(S, P, O, Graph)),
			writeq((S, P, O)),
			nl,
   			rdf_assert(S, P, O)
        ),
	_Triples),
	writeln('====\n===='),
	%writeq(Triples),
	nl.


process_task(Task) :-
	writeq(
		process_task(Task)
	),nl,
	
	(	rdf(Task, tc:source_file, (Task_source_file_name^^_))
	->	format("Processing testcase from file: ~w~n", [Task_source_file_name])
	;	true),

	rdf(Task, tc:kb_texts, Kb_texts),
	%writeq(Kb_texts),nl,writeln(yahooo),
	%ok, time to load the kbs.
	load_kbs(Kb_texts, _).
	
	
	%tc.query_text = text



load_kbs(Kb_texts, [Quad_list|Quad_lists]) :-
	rdf(Kb_texts, rdf:first, F),
	rdf(Kb_texts, rdf:rest, R),
	load_kb(F, Quad_list),
	load_kbs(R, Quad_lists).
	
load_kbs(Nil, []) :-
	rdf_equal2(rdf:nil, Nil).

load_kb(X, Quad_list) :-
	rdf(X, tc:text, Text),
	rdf(X, tc:base_uri, Base_uri),
	format("parse text>>>~n~w~n<<<~n", [Text]),
	call_with_string_read_stream(Text, {Base_uri, Quad_list}/[Stream]>>parse_n3_stream(Base_uri, Stream, Quad_list)).









% initialization() would never work along with dev_runner. That's just another hillarious feature of swipl.
%:- initialization(main).

