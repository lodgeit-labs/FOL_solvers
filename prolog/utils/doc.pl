/*
	a backtracking quad-store implemented with an open list stored in a global thread-local variable.
	also robust-specific prefix declarations, convenience functions, ...

	Since robust currently makes minimal to no use of the backtracking feature of this store, a big optimization might be to replace it with a non-backtracking implementation.

	The SWIPL RDF database itself is a bad candidate, as it's much slower in my tests.

*/



:- use_module(library(clpfd)).

% see also doc.pl RdfTemplates.trig agraph.py
:- rdf_register_prefix(code,				'https://rdf.lodgeit.net.au/v1/code#').
:- rdf_register_prefix(kb,					'https://rdf.lodgeit.net.au/v1/kb#').
:- rdf_register_prefix(l,					'https://rdf.lodgeit.net.au/v1/request#').
:- rdf_register_prefix(account_taxonomies,	'https://rdf.lodgeit.net.au/v1/account_taxonomies#').
:- rdf_register_prefix(accounts,			'https://rdf.lodgeit.net.au/v1/accounts#').
:- rdf_register_prefix(livestock,			'https://rdf.lodgeit.net.au/v1/livestock#').
:- rdf_register_prefix(excel,				'https://rdf.lodgeit.net.au/v1/excel#').
:- rdf_register_prefix(depr,				'https://rdf.lodgeit.net.au/v1/calcs/depr#').
:- rdf_register_prefix(ic,					'https://rdf.lodgeit.net.au/v1/calcs/ic#').
:- rdf_register_prefix(hp,					'https://rdf.lodgeit.net.au/v1/calcs/hp#').
:- rdf_register_prefix(div7a,				'https://rdf.lodgeit.net.au/v1/calcs/div7a#').
:- rdf_register_prefix(div7a_repayment,		'https://rdf.lodgeit.net.au/v1/calcs/div7a/repayment#').
:- rdf_register_prefix(depr_ui,				'https://rdf.lodgeit.net.au/v1/calcs/depr/ui#').
:- rdf_register_prefix(ic_ui,				'https://rdf.lodgeit.net.au/v1/calcs/ic/ui#').
:- rdf_register_prefix(hp_ui,				'https://rdf.lodgeit.net.au/v1/calcs/hp/ui#').
:- rdf_register_prefix(transactions,		'https://rdf.lodgeit.net.au/v1/transactions#').
:- rdf_register_prefix(s_transactions,		'https://rdf.lodgeit.net.au/v1/s_transactions#').
:- rdf_register_prefix(report_entries,		'https://rdf.lodgeit.net.au/v1/report_entries#').
:- rdf_register_prefix(smsf,				'https://rdf.lodgeit.net.au/v1/calcs/smsf#').
:- rdf_register_prefix(smsf_ui,				'https://rdf.lodgeit.net.au/v1/calcs/smsf/ui#').
:- rdf_register_prefix(smsf_distribution,	'https://rdf.lodgeit.net.au/v1/calcs/smsf/distribution#').
:- rdf_register_prefix(smsf_distribution_ui,'https://rdf.lodgeit.net.au/v1/calcs/smsf/distribution_ui#').
:- rdf_register_prefix(smsf_computation,	'https://rdf.lodgeit.net.au/v1/calcs/smsf/computation#').
:- rdf_register_prefix(reallocation,		'https://rdf.lodgeit.net.au/v1/calcs/ic/reallocation#').
:- rdf_register_prefix(bs,					'https://rdf.lodgeit.net.au/v1/bank_statement#').
:- rdf_register_prefix(av,					'https://rdf.lodgeit.net.au/v1/action_verbs#').
:- rdf_register_prefix(uv,					'https://rdf.lodgeit.net.au/v1/unit_values#').
:- rdf_register_prefix(phases,				'https://rdf.lodgeit.net.au/v1/phases#').


:- rdf_register_prefix(rdf,
'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(rdfs,
'http://www.w3.org/2000/01/rdf-schema#').


 rdf_equal2(X,Y) :-
	!rdf_global_id(X, X2),
	!rdf_global_id(Y, Y2),
	X2 = Y2.

 e(X,Y) :-
	rdf_equal2(X,Y).


%:- debug(doc).

% https://www.swi-prolog.org/pldoc/man?predicate=rdf_meta/1
/* uses goal_expansion, so as soon as you wrap the call in a !, it doesn't work, so we have to do this at runtime too anyway.
maybe this program will even run faster without this?*/
:- rdf_meta doc_add(r,r,r).
:- rdf_meta doc_add(r,r,r,r).
:- rdf_meta doc_assert(r,r,r,r).
:- rdf_meta doc(r,r,r).
:- rdf_meta doc(r,r,r,r).
:- rdf_meta doc_new_(r,-).
:- rdf_meta result_property(r,r).
:- rdf_meta rp(r,r).
:- rdf_meta result_add_property(r,r).
:- rdf_meta result_add_property(r,r,r).
:- rdf_meta result_assert_property(r,r,r).
:- rdf_meta result_assert_property(r,r,r,r).
:- rdf_meta doc_value(r,r,r).
:- rdf_meta doc_add_value(r,r,r).
:- rdf_meta doc_add_value(r,r,r,r).
:- rdf_meta t(r,r).

 'check that there is only one exception hook and it\'s ours' :-
	findall(
		Body,
		(
			clause(prolog_exception_hook(A,B,C,D),Body)
		),
		Xs),
	(	(
			Xs = [(user:doc_saving_prolog_exception_hook(A,B,C,D))]
		;
			Xs = [(doc_saving_prolog_exception_hook(A,B,C,D))]
		)
	->	true
	;	throw(internal_error(prolog_exception_hook(Xs)))).

 doc_init :-
	init_prolog_exception_hook,
	'check that there is only one exception hook and it\'s ours',
	(	nb_current(doc_trail_openend_file_output_stream, _)
	->	true
	;	doc_init_trace_0),
	%thread_create('watch doc-dumper command pipe', _),
	doc_clear.

 reestablish_doc(G,Ng) :-
	/* these two global vars together comprise the whole of doc database */
	b_setval(the_theory, G),
	b_setval(the_theory_nonground, Ng).



/*
good thing is i think even with retracts (the backtracking kind), we won't have to worry about prolog reusing variable numbers. anyway, variables are todo
*/

/* why 0? the idea was that there'd also be other, higher level formats */
 doc_init_trace_0 :-
	Fn = 'doc_trace_0.txt',
	Fnn = loc(file_name, Fn),
	(	absolute_tmp_path(Fnn, loc(absolute_path, Trail_File_Path))
	->	true
	;	Trail_File_Path = Fn),
	open(Trail_File_Path, write, Trail_Stream, [buffer(line)]),
	env_bool('ROBUST_DOC_ENABLE_TRAIL', Enabled),
	atomic_list_concat(['ROBUST_DOC_ENABLE_TRAIL: ',Enabled], Msg),
	writeln(Trail_Stream, Msg),
	b_setval(doc_trail_openend_file_output_stream, Trail_Stream).


flag_default('ROBUST_DOC_ENABLE_TRAIL', false).

:- if(env_bool('ROBUST_DOC_ENABLE_TRAIL', true)).

 doc_trace0(Term) :-
	b_getval(doc_trail_openend_file_output_stream, Stream),
	(	true %Stream \= []
	->	(
			writeq(Stream, Term),
			writeln(Stream, ',')
		)
	;	true).

:- else.

 doc_trace0(_) :- true.

:- endif.




 dump :-
	findall(_,
		(
			*doc(S,P,O),
			debug(doc, 'dump:~q~n', [(S,P,O)])
		),
	_).


 doc_default_graph(G) :-
	b_getval(default_graph, G).

 result_sheets_graph(result_sheets).

/*
┏┳┓┏━┓╺┳┓╻┏━╸╻ ╻╻┏┓╻┏━╸
┃┃┃┃ ┃ ┃┃┃┣╸ ┗┳┛┃┃┗┫┃╺┓
╹ ╹┗━┛╺┻┛╹╹   ╹ ╹╹ ╹┗━┛
*/

 doc_clear :-
	doc_trace0(doc_clear),
	b_setval(the_theory,subjs{}),
	b_setval(the_theory_nonground,[]),
	doc_set_default_graph(default).


 doc_set_default_graph(G) :-
	doc_trace0(doc_set_default_graph(G)),
	b_setval(default_graph, G).

 doc_add(S, [P, O|Rest]) :-
	doc_add(S,P,O),
	doc_add(S,Rest).

 doc_add(_, []).

 doc_add((S,P,O)) :-
	doc_add(S,P,O).


 doc_add(S,P,O) :-
  	%assertion(ground((S,P,O))),
	doc_default_graph(G),
	doc_add(S,P,O,G).


 atom_or_idk(X,X2) :-
	(	atom(X)
	->	X2 = X
	;	X2 = idk).

 get_or_default(T, X, XXX) :-
	(	XXX = T.get(X)
	->	true
	;	XXX = _{}).

 doc_add_gspo_no_global_id(G,(S,P,O)) :-
 	addd(S,P,O,G).
 	/*
 	another possible optimization might be to avoid calling rdf_global_id, because all the uris coming from parsing a rdf file are already global ids i think.
 	*/

 doc_add(S,P,O,G) :-
	rdf_global_id(S, S2),
	rdf_global_id(P, P2),
	rdf_global_id(O, O2),
	rdf_global_id(G, G2),
	doc_trace0(doc_add(S2,P2,O2,G2)),
	%debug(doc, 'add:~q~n', [(S2,P2,O2,G2)]),
	addd(S2,P2,O2,G2).

:- if(env_bool('ROBUST_DOC_ENABLE_TRAIL', true)).

 doc_add(S,P,O,G) :-
	doc_trace0(clean_pop(doc_add(S,P,O,G))),
	fail.

:- endif.

/*todo b_getval(the_theory_nongrounds,TTT),*/

/*
assumption: only O's are allowed to be non-atoms
*/

 addd(S2,P2,O2,G2) :-

 	/* these are used as keys to the dicts */
	atom(S2),atom(P2),atom(G2),
	!,

	% get the_theory global, ie a dict from subjects to pred-dicts
	b_getval(the_theory,Ss),

	% does it contain the subject?

	(	Ps = Ss.get(S2)
	%	it's a mapping from preds to graphs
	->	Ss2 = Ss
	;	(
			Ps = preds{},
			Ss2 = Ss.put(S2, Ps),
			b_setval(the_theory, Ss2)
		)
	),

	(	Gs = Ps.get(P2)
	->	Ps2 = Ps
	;	(
			Gs = graphs{},
			Ps2 = Ps.put(P2, Gs),
			b_set_dict(S2, Ss2, Ps2)
		)
	),

/*
todo this is an alternative ending, check if it's faster.

	(	Os = Gs.get(G2)
	->	(
			append(Os, [O2], Os2),
			Gs2 = Gs.put(G2, Os2),
			b_set_dict(P2, Ps2, Gs2)
		)
	;	(
			Gs2 = Gs.put(G2, [O2]),
			b_set_dict(P2, Ps2, Gs2)
		)
	).
*/

	(	Os = Gs.get(G2)
    ->      true
	;	(
            Os = _New_Rol,
            Gs2 = Gs.put(G2, Os),
            b_set_dict(P2, Ps2, Gs2))),
    rol_add(O2, Os).


 addd(S2,P2,O2,G2) :-
	\+((atom(S2),atom(P2),atom(G2))),
	!,
	X = spog(S2,P2,O2,G2),
	% adding non-ground triples is nonoptimal, because they aren't indexed.
	%format(user_error, 'ng:~q~n', [X]),
	b_getval(the_theory_nonground, Ng),
	append(Ng, [X], Ng2),
	b_setval(the_theory_nonground, Ng2).
	%rol_add(X, $>).

/*
dddd(Spog, X) :-
	Spog = spog(S2,P2,O2,G2),
	(atom(S2);var(S2)),
	(atom(P2);var(P2)),
	(atom(G2);var(G2)),
	member(O2, X.get(S2).get(P2).get(G2)).
*/
 dddd(Spog, X) :-
	Spog = spog(S2,P2,O2,G2),
	%(atom(S2);var(S2)), i dont think we need to allow this at all
	%(atom(P2);var(P2)), i dont think we need to allow this at all
	%(atom(G2);var(G2)), i dont think we need to allow this at all
	/* looks like a bug here not finding a triple if S2 is unbound? At any case, if any of S2, P2 or O2 are unbound, the yields are in random order, so we have to find another way than dicts. (and than lists, which were slow, or was that just the rol- stuff?. */
	rol_member(O2, X.get(S2).get(P2).get(G2)).

 dddd(Spog, _X) :-
	member(Spog, $>b_getval(the_theory_nonground)).







 doc_assert(S,P,O,G) :-
	doc_add(S,P,O,G).
/*	doc_trace0(doc_assert(S,P,O,G)),
	b_getval(the_theory,X),
	rol_assert(spog(S,P,O,G),X).*/

 doc_assert(S,P,O,G) :-
	doc_trace0(clean_pop(doc_assert(S,P,O,G))),
	fail.



/*

┏━┓╻ ╻┏━╸┏━┓╻ ╻╻┏┓╻┏━╸
┃┓┃┃ ┃┣╸ ┣┳┛┗┳┛┃┃┗┫┃╺┓
┗┻┛┗━┛┗━╸╹┗╸ ╹ ╹╹ ╹┗━┛
*/

/*
must have at most one match
not sure if this is followed? why not use determinancy checker?
*/
 doc((S,P,O)) :-
	doc(S,P,O).

 doc(S,P,O) :-
	doc_default_graph(G),
	doc(S,P,O,G).

/*
must have at most one match
*/
 doc(S,P,O,G) :-
	!(atom(S);var(S)),
	rdf_global_id(S, S2),
	rdf_global_id(P, P2),
	rdf_global_id(O, O2),
	rdf_global_id(G, G2),
	b_getval(the_theory,X),
	%debug(doc, 'doc?:~q~n', [(S2,P2,O2,G2)]),
	dddd(spog(S2,P2,O2,G2), X).

/*
member
*/

/*
 has(S,P,O) :-
	(	doc(S,P,O2)
	->	O = O2
	;	doc_add(S,P,O)).
*/
 doc_new_uri(Uri) :-
	doc_new_uri('', Uri).

 doc_new_uri(Postfix, Uri) :-
	result_data_uri_base(Result_data_uri_base),
	/* fixme, use something deterministic */
	gensym('x', Uri0),
	(	Postfix = ''
	->	atomic_list_concat([Result_data_uri_base, Uri0], Uri)
	;	atomic_list_concat([Result_data_uri_base, Uri0, '_', Postfix], Uri)).

% note: uniqueness is not checked, we rely on namespacing by Postfix
 bn(Postfix, Uri) :-
	doc_new_uri(Postfix, Uri).

/*
░░░░░░░░░░░░░█▀▄░█▀█░█░░
░░░░░░░░░░░░░█▀▄░█░█░█░░
░░░░░░░░░░░░░▀░▀░▀▀▀░▀▀▀
	Reasonably Open List.
	T is an open list. Unifying with the tail variable is only possible through rol_add.
*/

/*
 ensure Spog is added as a last element of T, while memberchk would otherwise possibly just unify an existing member with it
*/


flag_default('ROBUST_ROL_ENABLE_CHECKS', false).

:- if(env_bool('ROBUST_ROL_ENABLE_CHECKS', true)).

 rol_add(Spog,T) :-
	rol_member(Spog,T),
	throw(added_quad_matches_existing_quad).

:- endif.

 rol_add(Spog,T) :-
	memberchk(Spog,T).


 rol_assert(Spog,T) :-
	rol_member(Spog,T)
	->	true
	;	memberchk(Spog,T).

/*?*/
 rol_add_quiet(Spog,T) :-
		rol_member(Spog,T)
	->	true
	; 	memberchk(Spog,T).

/* nondet */
 rol_member(SpogA,T) :-
	/* avoid unifying SpogA with the open tail of T */
	member(SpogB, T),
	(	nonvar(SpogB)
	->	SpogA = SpogB
	;	(!,fail)).

	/*match(SpogA, SpogB)).
 match((S1,P1,O1,G1),(S2,P2,O2,G2))
	(	S1 = S2
	->	true
	;	rdf_equal(?Resource1, ?Resource2)
	*/

:- if(env_bool('ROBUST_ROL_ENABLE_CHECKS', true)).

 rol_single_match(T,SpogA) :-
	copy_term(SpogA,SpogA_Copy),
	rol_member(SpogA,T),
	(
		/* only allow one match */
		findall(x,rol_member(SpogA_Copy,T),Matches),
		length(Matches, Length),
		(	Length > 1
		->	(
				format(string(Msg), 'multiple_matches, use docm: ~q', [SpogA_Copy]),
				throw_string(Msg)
			)
		;	true)
	).

:- else.

 rol_single_match(T,SpogA) :-
	rol_member(SpogA,T).

:- endif.








/*
░█▀▀░█▀▄░█▀█░█▄█░░░█░▀█▀░█▀█░░░█▀▄░█▀▄░█▀▀
░█▀▀░█▀▄░█░█░█░█░▄▀░░░█░░█░█░░░█▀▄░█░█░█▀▀
░▀░░░▀░▀░▀▀▀░▀░▀░▀░░░░▀░░▀▀▀░░░▀░▀░▀▀░░▀░░
*/

 node_rdf_vs_doc(
	Float ^^ 'http://www.w3.org/2001/XMLSchema#decimal',
	Rat) :-
		/*freeze(Float, float(Float)),
		freeze(Rat, rational(Rat)),*/
		(
			(var(Float),rational(Rat)) /* g trace is totally baffled by this place, but the gist is that for anything else than a rational Rat, this correctly fails and goes on to the next case */
		;
			(var(Rat), float(Float))
		),
		(	nonvar(Rat)
		->	Float is float(Rat)
		;	Rat is rationalize(Float)),!.

 node_rdf_vs_doc(
	Float ^^ 'http://www.w3.org/2001/XMLSchema#double',
	Rat) :-
		(var(Rat), float(Float)),
		(	nonvar(Rat)
		->	Float is float(Rat)
		;	Rat is rationalize(Float)),!.

 node_rdf_vs_doc(
	String ^^ 'http://www.w3.org/2001/XMLSchema#string',
	String):- string(String), !.

 node_rdf_vs_doc(
	X ^^ 'http://www.w3.org/2001/XMLSchema#boolean',
	X) :-
	(X == true
	;
	X == false),
	!.
% boolean has to be above atom as a special case of atomic values
 node_rdf_vs_doc(Atom, Atom) :- atom(Atom),!.

 node_rdf_vs_doc(
	date(Y,M,D) ^^ 'http://www.w3.org/2001/XMLSchema#date',
	date(Y,M,D)) :- !.

 node_rdf_vs_doc(
	date_time(Y,M,D,Z0,Z1,Z2) ^^ 'http://www.w3.org/2001/XMLSchema#dateTime',
	date(Y,M,D)) :-
		is_zero_number(Z0),
		is_zero_number(Z1),
		is_zero_number(Z2),!.

 node_rdf_vs_doc(
	date_time(Y,L,D,H,M,S) ^^ 'http://www.w3.org/2001/XMLSchema#dateTime',
	date(Y,L,D,H,M,S, 0,'UTC',-)) :- !.

 node_rdf_vs_doc(
	Int ^^ 'http://www.w3.org/2001/XMLSchema#integer',
	Int) :- integer(Int),!.

 node_rdf_vs_doc(String, Term) :-
	var(String),
	String = String2^^'http://www.w3.org/2001/XMLSchema#string',
	/*compound(Term), */term_string(Term, String2),
	!.







 triple_rdf_vs_doc((S,P,O), (S,P,O2)) :-
	(var(S);atom(S)),!,
	(	catch(
			node_rdf_vs_doc(O,O2),
			E,
			(
				format(user_error, '~q', [E]),
				throw_string(E)
			)
		)
	->	true
    ;	throw_string($>format(string(<$), 'conversion from rdf to doc failed: ~q <-> ~q', [O,O2]))
    ).

	

/* todo vars */


%:- debug(doc).

 doc_to_rdf(Rdf_Graph) :-
	rdf_create_bnode(Rdf_Graph),
	findall(_,
		(
			%*doc(X,Y,Z),
			*doc(T),
			round_term(T,T2),
			%debug(doc, 'to_rdf:~q~n', [T2]),
			triple_rdf_vs_doc((X2,Y2,Z2),T2),
			!rdf_assert(X2,Y2,Z2,Rdf_Graph)
		),_).

 add_to_rdf((X,Y,Z,G)) :-
	(
		round_term((X,Y,Z),T),
		triple_rdf_vs_doc((X2,Y2,Z2),T),
		%debug(doc, 'to_rdf:~q~n', [(X2,Y2,Z2,G)]),
		catch(
			rdf_assert(X2,Y2,Z2,G),
			E,
			format(user_error,'~q~n -while saving triple:~n~q~n',[E, (X,Y,Z,G)])
		)
	)
	->	true
	;	format(user_error, 'add_to_rdf failed on: ~q~n', [(X,Y,Z,G)]).


/*:- comment(lib:doc_to_rdf_all_graphs, "if necessary, modify to not wipe out whole rdf database and to check that G doesn't already exist */


 doc_to_rdf_all_graphs :-
	doc_to_rdf_graph(_).

 doc_to_rdf_graph(G) :-
 	rdf_retractall(_,_,_,_),
	findall(_,(
			*doc(X,Y,Z,G),
			add_to_rdf((X,Y,Z,G))
		),_
	).


 save_doc_(Format, Id, Report_key) :-
	atomic_list_concat(['doc_', Id, '.', Format],Fn),
	report_file_path(loc(file_name, Fn), Url, loc(absolute_path,Path)),
	Url = loc(absolute_url, Url_Value),
    %misc_check(!parse_url(Url_Value, _)), % ipv6 syntax is not supported yet, apparently
	(	var(Report_key)
	->	Report_key = Fn
	;	true),
	add_report_file(-15, Report_key, Fn, Url),
	Options = [
		sorted(true),
		base(Url_Value),
		canonize_numbers(true),
		abbreviate_literals(false),
		prefixes([rdf,rdfs,xsd,l,livestock,excel,r-($>atomic_list_concat([$>result_data_uri_base, '#']))])
	],
	(	Format = trig
	->	!rdf_save_trig(Path, Options)
	;	!rdf_save_turtle(Path, Options)).
% ^
%todo refactor
% v
 make_rdf_response_report :-
	Title = 'response.n3',
	!doc_to_rdf(Rdf_Graph),
	!report_file_path(loc(file_name, Title), Url, loc(absolute_path,Path)),
	!add_report_file(-11,Title, Title, Url),
	Url = loc(absolute_url, Url_Value),
	!rdf_save_turtle(Path, [graph(Rdf_Graph), sorted(true), base(Url_Value), canonize_numbers(true), abbreviate_literals(false), prefixes([rdf,rdfs,xsd,l,livestock])]).

 save_doc(Id) :-
	doc_to_rdf_all_graphs,
	save_doc_(turtle, Id, _),
	save_doc_(trig, Id, _),
	rdf_retractall(_,_,_,_).


 save_doc_graph(Graph, Report_key) :-
	doc_to_rdf_graph(Graph),
	save_doc_(turtle, Report_key, Report_key).


 add_result_sheets_report(Graph) :-
	save_doc_graph(Graph, result_sheets).



 doc_from_rdf(Rdf_Graph, Replaced_prefix, Replacement_prefix) :-
 	/*
 	all graphs are mashed into default graph. That means no triple metadata is preserved, but that's ok, because we don't use it anyway.
 	*/
 	doc_default_graph(Default_graph),

 	/*
 	a fun optimization might be:
 	1) figure out a rdf format that's probably binary, plain quad list, and suitable for partitioning.
 	2) concurrent_maplist the loading, and the below logic, one thread for one partition, up until doc_add (which has to be run sequentially)
 	*/

	findall((X2,Y2,Z2),
		(
			rdf(X,Y,Z,Rdf_Graph),
			replace_uri_node_prefix(X, Replaced_prefix, Replacement_prefix, X2),
			replace_uri_node_prefix(Y, Replaced_prefix, Replacement_prefix, Y2),
			replace_uri_node_prefix(Z, Replaced_prefix, Replacement_prefix, Z2)
		),
		Triples),
	maplist(triple_rdf_vs_doc, Triples, Triples2),
	maplist(doc_add_gspo_no_global_id(Default_graph), Triples2).

 replace_uri_node_prefix(Z, Replaced_prefix, Replacement_prefix, Z2) :-
	(	(
			atom(Z),
			atom_prefix(Z, Replaced_prefix)
		)
	->	!replace_atom_prefix(Z, Replaced_prefix, Replacement_prefix, Z2)
	;	Z = Z2).

 replace_atom_prefix(X, Replaced_prefix, Replacement_prefix, X2) :-
	atom_length(Replaced_prefix, L0),
	atom_length(X, L1),
	L2 is L1 - L0,
	sub_atom(X,_,L2,0,S),
	atomic_list_concat([Replacement_prefix, S], X2).


/*
░░▄▀░█▀▀░█▀█░█▀█░█░█░█▀▀░█▀█░▀█▀░█▀▀░█▀█░█▀▀░█▀▀░▀▄░
░▀▄░░█░░░█░█░█░█░▀▄▀░█▀▀░█░█░░█░░█▀▀░█░█░█░░░█▀▀░░▄▀
░░░▀░▀▀▀░▀▀▀░▀░▀░░▀░░▀▀▀░▀░▀░▀▀▀░▀▀▀░▀░▀░▀▀▀░▀▀▀░▀░░
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
*/


 doc_new_(Type, Uri) :-
	doc_new_uri(Uri),
	doc_add(Uri, rdf:type, Type).

 doc_new_theory(T) :-
	doc_new_uri(T),
	doc_add(T, rdf:type, l:theory).

 request_data_property(P, O) :-
	request_data(Request_Data),
	doc(Request_Data, P, O).

 report_details_property_value(P, V) :-
	!report_details(Details),
	doc_value(Details, P, V).

 rp(P, O) :-
 	result_property(P, O).

 result_property(P, O) :-
	result(R),
	doc(R, P, O).

 result_add_property(P, O) :-
	doc_default_graph(G),
	result_add_property(P, O, G).

 result_add_property(P, O, G) :-
	result(R),
	doc_add(R, P, O, G).

 result_assert_property(P, O) :-
	doc_default_graph(G),
	result_assert_property(P, O, G).

 result_assert_property(P, O, G) :-
	result(R),
	doc_assert(R, P, O, G).




:- table request/1.
 request(R) :-
	doc(R, rdf:type, l:'Request').

:- table result/1.
 result(R) :-
	!doc(R, rdf:type, l:'Result').
 	%format(user_error, 'result(R): ~q~n', [R]).

:- table request_data/1.
 request_data(D) :-
	!request(Request),
	!doc(Request, l:has_request_data, D).

:- table result_data_uri_base/1.
 result_data_uri_base(B) :-
 	rp(l:has_result_data_uri_base, B).



 result_accounts(As) :-
	result(D),
	!doc(D, l:has_accounts, As).


 add_alert_stringified(Type, Msg) :-
	term_string(Msg, Str),
 	add_alert(Type, Str, _).

 add_alert(Type, Msg) :-
 	add_alert(Type, Msg, _).

 add_alert(Type, Msg, Uri) :-
	result(R),
	context_string(Ctx_str),
	doc_new_uri(alert, Uri),
	doc_add(R, l:alert, Uri),

	doc_add(Uri, [
		l:type, 	Type,
	 	l:message, 	Msg,
	 	l:ctx_str, 	Ctx_str
	]).


 assert_alert(Type, Msg) :-
	/*todo*/
	result(R),
	doc_new_uri(alert, Uri),
	doc_add(R, l:alert, Uri),
	doc_add(Uri, l:type, Type),
	doc_add(Uri, l:message, Msg).

 get_alert(Type, Msg, Str, Uri) :-
	result(R),
	*doc(R, l:alert, Uri),
	doc(Uri, l:type, Type),
	doc(Uri, l:plain_message, Msg),
	doc(Uri, l:message, Str).

 add_comment_stringize(Title, Term) :-
	pretty_term_string(Term, String),
	add_comment_string(Title, String).

 add_comment_string(Title, String) :-
	doc_new_uri(comment, Uri),
	doc_add(Uri, title, Title, comments),
	doc_add(Uri, body, String, comments).

 doc_list_member(M, L) :-
	doc(L, rdf:first, M).

 doc_list_member(M, L) :-
	doc(L, rdf:rest, R),
	doc_list_member(M, R).

 doc_list_items(L, Items) :-
	findall(Item, doc_list_member(Item, L), Items).

 doc_add_list(Prolog_list, Uri) :-
	doc_add_list(Prolog_list, $>doc_default_graph, Uri).

 doc_add_list([H|T], G, Uri) :-
	doc_new_uri(rdf_list, Uri),
	doc_add(Uri, rdf:first, H, G),
	doc_add_list(T, G, Uri2),
	doc_add(Uri, rdf:rest, Uri2, G).

 doc_add_list([], _G, rdf:nil).

 doc_value(S, P, V) :-
	doc_default_graph(G),
	doc_value(S, P, V, G).

 doc_value(S, P, V, G) :-
 	doc(S, P, O, G),
 	doc(O, rdf:value, V).

 value(O,V) :-
 	doc(O, rdf:value, V).

 values(Os,Vs) :-
 	maplist(value, Os, Vs).


 doc_add_value(S, P, V) :-
 	doc_default_graph(G),
 	doc_add_value(S, P, V, G).

 doc_add_value(S, P, V, G) :-
	doc_add_value2(S, P, _Uri, V, G).

 doc_add_value2(S, P, Uri, V) :-
 	doc_default_graph(G),
	doc_add_value2(S, P, Uri, V, G).

 doc_add_value2(S, P, Uri, V, G) :-
 	doc_new_uri(value, Uri),
 	doc_add(S, P, Uri, G),
 	doc_add(Uri, rdf:value, V, G).



/*
user:goal_expansion(
	vague_props(X, variable_names(Names))
, X) :-
	term_variables(X, Vars),
	maplist(my_variable_naming, Vars, Names).
vague_doc(S,
	compile_with_variable_names_preserved(X, variable_names(Names))),
*/

/*

pondering a syntax for triples..

	R l:ledger_account_opening_balance_total [
		l:coord Opening_Balance;
		l:ledger_account_name Bank_Account_Name];
	  l:ledger_account_opening_balance_part [
	  	l:coord Opening_Balance_Inverted;
		l:ledger_account_name Equity];
	*/
	/*
	R l:ledger_account_opening_balance_total [
		l:coord Opening_Balance;
		l:ledger_account_name Bank_Account_Name];
	  l:ledger_account_opening_balance_part [
	  	l:coord $>coord_inverse(<$, Opening_Balance),
		l:ledger_account_name $>account_by_role('Accounts'/'Equity')];
*/



 gu(Prefixed, Full) :-
	rdf_global_id(Prefixed, Full).






 t(X,Y) :-
	!doc(X, rdf:type, Y).




/*
 ┏╸╻ ╻┏┳┓╻     ╺┳╸┏━┓   ╺┳┓┏━┓┏━╸╺┓
╺┫ ┏╋┛┃┃┃┃      ┃ ┃ ┃    ┃┃┃ ┃┃   ┣╸
 ┗╸╹ ╹╹ ╹┗━╸╺━╸ ╹ ┗━┛╺━╸╺┻┛┗━┛┗━╸╺┛
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
*/


/*
represent xml in doc.
*/

 request_xml_to_doc(Dom) :-
	xml_to_doc(request_xml, [
		balanceSheetRequest,
		unitValues
	], pid:request_xml, Dom).

 xml_to_doc(Prefix, Uris, Root, Dom) :-
	b_setval(xml_to_doc_uris, Uris),
	b_setval(xml_to_doc_uris_prefix, Prefix),
	b_setval(xml_to_doc_uris_used, _),
	xml_to_doc(Root, Dom).

 xml_to_doc(Root, Dom) :-
	maplist(xml_to_doc(Root), Dom).

 xml_to_doc(Root, X) :-
	atomic(X),
	doc_add(Root, rdf:value, X).

 xml_to_doc(Root, element(Name, _Atts, Children)) :-
	b_getval(xml_to_doc_uris, Uris),
	b_getval(xml_to_doc_uris_used, Used),
	b_getval(xml_to_doc_uris_prefix, Prefix),

	(	member(Name, Uris)
	->	(	rol_member(Name, Used)
		->	throw_string('tag with name supposed to be docified as an uri already appeared')
		;	(
				Uri = Prefix:Name,
				rol_add(Name, Used)
			)
		)
	;	doc_new_uri(Uri)),

	doc_add(Root, Name, Uri),
	xml_to_doc(Uri, Children).








/*

╺┳┓┏━╸┏┓ ╻ ╻┏━╸┏━╸┓┏┓╻┏━╸
 ┃┃┣╸ ┣┻┓┃ ┃┃╺┏┃╺┓┃┃┗┫┃╺┓
╺┻┛┗━╸┗━┛┗━┛┗━┛┗━┛╹╹ ╹┗━┛


*/

 'watch doc-dumper command pipe' :-
    %shell4('mkfifo fo',_),
    File = 'control/fo',
    (	access_file(File, exist)
    ->	true
    ;	shell($>atom_concat('mkfifo ', File), _)),
    open('control/fo', read, Fo),
    read_term(Fo, X,[]),
    open(X,append,Out_Stream),
    writeq(Out_Stream, 'asking main thread to dump doc..'),
    close(Out_Stream),
    thread_signal(main, doc_dump),
    'watch doc-dumper command pipe'.

%:- initialization(thread_create('watch doc-dumper command pipe', _)).

 doc_dump :-
	gensym(dump, Id),
	once(save_doc(Id)).

 dg :-
	doc_dump,gtrace.





/*
we could control this with a thread select'ing some unix socket
*/
/*doc_dumping_enabled :-
	flag(doc_dumping_enabled, true).
*/





/*

diff from rol_ version. This was maybe even faster, and prolly uses a lot less memory?

@@ -169,12 +169,16 @@ addd(S2,P2,O2,G2) :-
        ),

        (       Os = Gs.get(G2)
-       ->      true
+       ->      (
+                       append(Os, [O2], Os2),
+                       Gs2 = Gs.put(G2, Os2),
+                       b_set_dict(P2, Ps2, Gs2)
+               )
        ;       (
-                       Os = _New_Rol,
-                       Gs2 = Gs.put(G2, Os),
-                       b_set_dict(P2, Ps2, Gs2))),
-       rol_add(O2, Os).
+                       Gs2 = Gs.put(G2, [O2]),
+                       b_set_dict(P2, Ps2, Gs2)
+               )
+       ).

 addd(S2,P2,O2,G2) :-
        \+((ground(spog(S2,P2,O2,G2)),atom(S2),atom(P2),atom(G2))),
@@ -186,7 +190,7 @@ dddd(Spog, X) :-
        (atom(S2);var(S2)),
        (atom(P2);var(P2)),
        (atom(G2);var(G2)),
-       rol_member(O2, X.get(S2).get(P2).get(G2)).
+       member(O2, X.get(S2).get(P2).get(G2)).
*/





/*

	permanent storage of doc data / request data in a triplestore:


--------

dereferenceable uris.
RDF_URI_BASE =


objects:
	request
	processing


prolog could need access to the request data







		we want hassle-free data exchange between microservices, but speed is a big concern, so probably the store should be in the prolog process. But the process may not be running at this point. So here, we maybe stick this data into rdflib, and pass that along, then possibly invoke a series of commands on prolog, the first inserting the data into it, the second executing the query









*/

/*

what makes sense to rdf-ize?
take, as an example account role. (RoleParent/RoleChild), posibly nested. For all purposes, this is atomic. We don't want to reference any sub-part of it, ever. It's assembled once, and then treated as an atom. If we "export" it into rdf as an uri, then it's also a referenceable identifier.

*/

/*
about namespaces:
	i think it'll be ideal if a contracted form is the default and used everywhere, ie, nodes are normalized into the contracted form when put into the store or queried.
	- this is what i'm trying out in QuadLad now
*/






:- dynamic(exception_doc_dump/2).
:- dynamic(exception_ctx_dump/1).

/*
problem with doing this along with using library(prolog_stack):
only one prolog_exception_hook is ever called. So, we'll save the prolog_stack clause, retract it, and call it ourselves.
This workaround worked most of the time, but not always, in V8.1.15. In versions 8.3, it never seems to work.

so, option 0: try to isolate the issue: prolog_exception_hook is sometimes ignored?

option 1: give up library(prolog_stack). Given how unreadable the stack usually is, this doesn't seem to be a big loss. When there's a problem, i gtrace anyway. I hoped the stack would be a "last resort" for users, once it would be presented in the rdf viewer, with clickable/explorable resurce uris.. But i guess i can focus on making sure that everything has a nice execution_stack..

option 2: thread a global State through everything. Would DCGs make that tolerable? if every rule was something like
rule --> {
	bla,
	bla
	},
would we get OldState->NewState threaded implicitly everywhere? Or some other macro lib? eeek.
Anyway, we could store both doc and context in State.

*/

:- dynamic(prolog_stack__prolog_exception_hook/4).
:- dynamic(prolog_exception_hook/4).
:- dynamic(doc_saving_prolog_exception_hook_is_inited/0).

 'save old prolog exception hook' :-

 	findall(hook((A,B,C,D),Body), clause(prolog_exception_hook(A,B,C,D),Body), Old_hooks),
	length(Old_hooks, Old_hooks_len),
	(	Old_hooks_len #> 1
	->	throw('Old_hooks_len #> 1')
	;	true),

	(	Old_hooks_len #= 1
	->	(
			Old_hooks = [hook((A,B,C,D),Body)],
			assert(prolog_stack__prolog_exception_hook(A,B,C,D) :- Body),
			%gtrace,
			retractall(prolog_exception_hook(A,B,C,D))
		)
	;	true).

 init_prolog_exception_hook :-
 	(	doc_saving_prolog_exception_hook_is_inited
 	->	true
 	;	(
		assert(doc_saving_prolog_exception_hook_is_inited),
		'save old prolog exception hook',
		assert(prolog_exception_hook(E,F, Frame, CatcherFrame) :- doc_saving_prolog_exception_hook(E,F, Frame, CatcherFrame))
	)).

%:- initialization(init_prolog_exception_hook).

 doc_saving_prolog_exception_hook(E,F, Frame, CatcherFrame) :-
	%print_message(information, "prolog_stack__prolog_exception_hook"),

	(	prolog_stack__prolog_exception_hook(E,F,Frame,CatcherFrame)
	->	true
	;	F = E),

	%print_message(information, "................."),
	format(user_error, 'doc_saving_prolog_exception_hook was invoked with exception ~q~n~n', [F]),
	%backtrace(200),

	% a big potential problem here is running into some code (like a library we need) that makes extensive use of exceptions. Each exception triggers this. Can we meaningfually check CatcherFrame maybe?

	catch('store doc data for reporting after exception',E,format(user_error,'~q~n',[E])),
	%print_message(information, "........."),
	catch('store ctx data for reporting after exception',E,format(user_error,'~q~n',[E])),

	%print_message(information, "."),
	true.
/*
*/

 doc_data(G,Ng) :-
	catch(
		(
			b_getval(the_theory, G),
			b_getval(the_theory_nonground, Ng)
		),
		_,
		false
	).

 'store doc data for reporting after exception' :-
	(	doc_data(G,Ng)
	->	(
			retractall(user:exception_doc_dump(_,_)),
			assert(user:exception_doc_dump(G,Ng)),
			nicety(doc_dump)
		)
	;	true).

 'store ctx data for reporting after exception' :-
	get_context(Ctx_list),
	%print_message(information, 'storing context:'(Ctx_list)),
	retractall(user:exception_ctx_dump(_)),
	assert(user:exception_ctx_dump(Ctx_list)).


/*
┏━┓┏━┓╻ ╻
┣┳┛┣━┛┃┏┛
╹┗╸╹  ┗┛
Required Property Value
*/

 rpv(S,P,V) :-
	(	doc(S,P,V0)
	->	!doc(V0, rdf:value, V)
	;
		/* throw an error */
		(
			(	doc(P, rdfs:label, Label)
			->	true
			;	rdf_global_id(Label, P)),
			throw_format('missing ~q of item in ~w.',
				[
					Label,
					$>sheet_and_cell_string(S)
				]
			)
		)
	).

 opv(S,P,V) :-
		doc(S,P,V0)
	->	!doc(V0, rdf:value, V).






/*
┏━╸╻ ╻┏━╸┏━╸╻
┣╸ ┏╋┛┃  ┣╸ ┃
┗━╸╹ ╹┗━╸┗━╸┗━╸
*/
 sheet_and_cell_string_for_property(Item, Prop, Str) :-
	!doc(Item, Prop, Value),
	!sheet_and_cell_string(Value, Str).

 sheet_and_cell_string(Value, Str) :-
	(	doc(Value, excel:has_sheet_name, Sheet_name)
	->	(	(doc(Value, excel:col, Col), doc(Value, excel:row, Row))
		->	!atomics_to_string(['sheet "', Sheet_name, '", cell ', Col, ':', Row], Str)
		;	!atomics_to_string(['sheet "', Sheet_name], Str))
	;	Str = "unknown location").

 read_coord_vector_from_doc_string(Item, Prop, Default_currency, Side, VectorA) :-
	doc_value(Item, Prop, Amount_string),
	(	vector_from_string(Default_currency, Side, Amount_string, VectorA)
	->	true
	;	throw_string(['error reading "amount" in ', $>!sheet_and_cell_string($>doc(Item, Prop))])).

 read_value_from_doc_string(Item, Prop, Default_currency, Value) :-
	doc_value(Item, Prop, Amount_string),
	(	value_from_string(Default_currency, Amount_string, Value)
	->	true
	;	(
			assertion(var(Value)),
			throw_string(['error reading "amount" in ', $>!sheet_and_cell_string($>doc(Item, Prop))])
		)
	).




 get_sheet(Type, Sheet) :-
	!doc($>request_data, excel:has_sheet_instances, Sheets),
	*doc_list_member(Sheet, Sheets),
	?doc(Sheet, excel:sheet_instance_has_sheet_type, Type).

 get_sheets(Type, Sheets) :-
 	findall(Sheet, get_sheet(Type, Sheet), Sheets).

 get_singleton_sheet(Type, Sheet) :-
 	get_sheets(Type, Sheets),
 	(	Sheets = [Sheet]
 	->	true
 	;	((	Sheets = []
 		->	throw_format('not expected: no sheets of type ~q', [Type])
 		;	true),
	 	throw_format('not expected: multiple sheets of type ~q', [Type]))).

 get_sheet_data(Type, Data) :-
	*get_sheet(Type, Sheet),
	!doc(Sheet, excel:sheet_instance_has_sheet_data, Data),
	!doc_add(Sheet, l:was_processed, true). % todo check.


 get_sheets_data(Type, Datas) :-
 	findall(Data, get_sheet_data(Type, Data), Datas).

 get_singleton_sheet_data(Type, Data) :-
 	get_sheets_data(Type, Datas),
 	(	Datas = [Data]
 	->	true
 	;	((	Datas = []
 		->	throw_format('not expected: no sheets of type ~q', [Type])
 		;	true),
	 	throw_format('not expected: multiple sheets of type ~q', [Type]))).

 get_optional_singleton_sheet_data(Type, Data) :-
 	get_sheets_data(Type, Datas),
 	length(Datas, L),
 	(	L #< 2
 	->	true
 	;	throw_format('not expected: multiple sheets of type ~q', [Type])),
 	?(Datas = [Data]).

 get_optional_singleton_sheet(Type, Sheet) :-
 	get_sheets(Type, Sheets),
 	length(Sheets, L),
 	(	L #< 2
 	->	true
 	;	throw_format('expected only one sheet of type ~q', [Type])),
 	Sheets = [Sheet].


