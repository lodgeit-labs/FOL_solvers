
:- ['../utils/exceptions.pl'].
:- ['../determinancy_checker/determinancy_checker_main.pl'].

:- multifile delay/2.
:- dynamic pyco3_rule/7.
:- discontiguous pyco3_rule/7.
:- multifile r/1.

%:- use_module(library(semweb/rdf11),except(['{}'/1])).
:- use_module(library(fnotation)).
:- fnotation_ops($>,<$).
:- op(900,fx,<$).




/*
compile-time clause expansion, produces 'pyco3_rule' terms from 'r' declarations
*/

:- ['../utils/compile_with_variable_names_preserved.pl'].

user:term_expansion(P, pyco3_rule(Id, Head, Body, Notes, Cnls, Names, Preps)) :-
	P =.. [r|X],
	gensym(r, Id),
	term_variables(P, Vars),
	maplist(try_get_variable_naming, Vars, Names),
	p_decl_to_rule2(X, Head, Body, Notes, Cnls, Preps),
	assertion(nonvar(Head)).

try_get_variable_naming(Var, (Name = Var)) :-
	var_property(Var, name(Name)),
	!.
try_get_variable_naming(Var, ('_' = Var)).


/*
expand the declarations.
*/

/*
make "bc" a note. "bc" stands for "base case". Our code doesn't make any use of this information for now.
*/
p_decl_to_rule2(
	[H|T],
	Head,
	Body,
	Notes,
	Cnls,
	Preps
) :-
	H = bc,!,
	p_decl_to_rule2(
		[n - bc|T],
		Head,
		Body,
		Notes,
		Cnls,
		Preps
	).

/* collect notes */
p_decl_to_rule2([H|T], Head, Body, [Note|Notes], Cnls, Preps) :-
	H = n - Note,!,
	p_decl_to_rule2(T, Head, Body, Notes, Cnls, Preps).

/* collect cnl syntaxes */
p_decl_to_rule2([H|T], Head, Body, Notes, [(Lang - Cnl)|Cnls], Preps) :-
	H = lang(Lang) - Cnl,!,
	p_decl_to_rule2(T, Head, Body, Notes, Cnls, Preps).

/* collect head with existential */
p_decl_to_rule2([H|T], Head, Body, Notes, Cnls, Preps) :-
	var(Head),
	existential_head_and_prep(H, Head, Prep),
	!,
	p_decl_to_rule2([prep - Prep|T], Head, Body, Notes, Cnls, Preps).

/* collect head */
p_decl_to_rule2([H|T], Head, Body, Notes, Cnls, Preps) :-
	var(Head),!,
	flatten([H], Head),
	(member((exists-_), Head)->throw_string(err);true),
	p_decl_to_rule2(T, Head, Body, Notes, Cnls, Preps).

/* collect prep */
p_decl_to_rule2([H|T], Head, Body, Notes, Cnls, [Prep|Preps]) :-
	H = prep - Prep,
	p_decl_to_rule2(T, Head, Body, Notes, Cnls, Preps).

/* collect body items */
p_decl_to_rule2([H|T], Head, [Body_head|Body_tail], Notes, Cnls, Preps) :-
	Body_head = H,
	p_decl_to_rule2(T, Head, Body_tail, Notes, Cnls, Preps).

p_decl_to_rule2([], _Head, [], [], [], []).


/* existential head rule expansion */
existential_head_and_prep(H, Head, Prep) :-
	H = exists - (Name, Properties),
	Prep = mkbn(Bn, Dict),

	maplist(property_value_variable, Properties, Values),
	existential_head_and_prep2(Name, Bn, Properties, Values, Head2, Kvs),


	/* for example 'fr', 'verb'.. */
	Head_first_item =.. [Name, Bn | Values],

	Head = [Head_first_item | Head2],
	dict_create(Dict, Name, Kvs).

/* produce a new Value variable for each property pred */
property_value_variable(_Property, _Value).

existential_head_and_prep2(Name, Bn, [Property|Properties], [Value|Values], [Head_item|Head_items], [Kv_pair|Kv_pairs]) :-
	Head_item =.. [$>atomic_list_concat([Name, '_', Property]), Bn, Value],
	Kv_pair = Property - Value,
	existential_head_and_prep2(Name, Bn, Properties, Values, Head_items, Kv_pairs).

existential_head_and_prep2(_, _, [], [], [], []).




/*
main entrypoint
*/
run(Quiet, Query) :-
	b_setval(bn_log, []),
	nb_setval(step, 0),
	run2(Quiet, Query).


/*
repeat top-level query until depth_map of Proof stops changing, then finalize
*/
run2(Quiet, Query) :-
	run2_repeat(Quiet, Query, Proof),
	debug(pyco_run, '~w final...', [$>trace_prefix(r, -1)]),
	/* filter out proofs that didn't ground. In these cases, we only got here due to ep_yield'ing.	*/
	run2_final(Query, Proof).


run2_repeat(Quiet, Query, Proof) :-
	debug(pyco_map, 'map0 for: ~q', [Proof]),
	depth_map(Proof, Map0),
	debug(pyco_map, 'map0 : ~q', [Map0]),
	proof([],0,eps{},ep_yield,Quiet,Query,Proof),
	debug(pyco_map, 'map1 for: ~q', [Proof]),
	depth_map(Proof, Map1),
	debug(pyco_map, 'map1 : ~q', [Map1]),
	((	Map0 \= Map1,
		debug(pyco_run, '~w repeating.', [$>trace_prefix(r, -1)]),
		run2_repeat(Quiet, Query, Proof),
		debug(pyco_run, '~w ok...', [$>trace_prefix(r, -1)])
	)
	;
	(	Map0 = Map1,
		debug(pyco_run, '~w stabilized.', [$>trace_prefix(r, -1)])
	)).

run2_final(Query, Proof) :-
	proof([],0,eps{},ep_fail,noisy,Query,Proof),
	debug(pyco_run, '~w result.', [$>trace_prefix(r, -1)]),
	true.


/* for debugging, try finalizing again and print that it failed */
run2_final(Query, Proof) :-
	\+proof([],0,eps{},ep_fail,quiet,Query,Proof),
	debug(pyco_run, '~w failed.', [$>trace_prefix(r, -1)]),
	fail.


proof(
	/* a unique path in the proof tree */
	Path,
	/* depth, incremented on each 'proof' recursion */
	Level,
	/* current ep list */
	Eps0,
	/* ep_yield or ep_fail */
	Ep_yield,
	/* silence debugging */
	Quiet,
	/* */
	Query,
	/* a tree of body items*/
	Proof
) :-
	nb_getval(step, Proof_id),
	term_string(Proof_id, Proof_id_str),
	Deeper_level is Level + 1,
	proof2(Path, Proof_id_str,Deeper_level,Eps0,Ep_yield,Quiet,Query,Proof).

proof2(Path0, Proof_id_str,Level,Eps0,Ep_yield, Quiet,Query,Proof) :-
	matching_rule(Level, Query, Desc, Body_items, Preps, Query_ep_terms, Head_item_idx),
	(Quiet = noisy -> debug(pyco_proof, '~w match: ~q (~q)', [$>trace_prefix(Proof_id_str, Level), $>nicer_term(Query), Desc]); true),

	append(Path0, [ri(Desc, Head_item_idx)], Path),
	register_frame(Proof_id_str),

	ep_list_for_rule(Eps0, Desc, Ep_List),
	(Quiet = noisy -> ep_debug_print_1(Ep_List, Query_ep_terms); true),
	proof3(Path, Proof_id_str, Eps0, Ep_List, Query_ep_terms, Desc, Preps, Level, Body_items, Ep_yield, Quiet, Query, Proof).

proof2(_Path, Proof_id_str,Level,_,_,Quiet,Query, call) :-
	call_native(Proof_id_str,Level, Quiet, Query).

proof3(Path, Proof_id_str, Eps0, Ep_List, Query_ep_terms, Desc, Preps, Level, Body_items, Ep_yield, Quiet, _Query, Proof) :-
	ep_ok(Ep_List, Query_ep_terms, Quiet),
	prove_body(Path, Proof_id_str, Ep_yield, Eps0, Ep_List, Query_ep_terms, Desc, Preps, Level, Body_items, Quiet, Proof).

proof3(Path, Proof_id_str, _Eps0, Ep_List, Query_ep_terms, Desc, _Prep, Level, _Body_items, Ep_yield, Quiet, Query, Proof) :-
	\+ep_ok(Ep_List, Query_ep_terms, Quiet),
	proof_ep_fail(Path, Proof_id_str, Desc, Level, Ep_yield, Quiet, Query, Proof).

proof_ep_fail(_Path, Proof_id_str, Desc, Level, Ep_yield, Quiet, Query, _Unbound_Proof) :-
	Ep_yield == ep_yield,
	(Quiet = noisy -> debug(pyco_proof, '~w ep_yield: ~q (~q)', [$>trace_prefix(Proof_id_str, Level), $>nicer_term(Query), Desc]); true),
	true.

proof_ep_fail(_Path, Proof_id_str, Desc, Level, Ep_yield, Quiet, Query, _Unbound_Proof) :-
	Ep_yield == ep_fail,
	(Quiet = noisy -> debug(pyco_proof, '~w ep_fail: ~q (~q)', [$>trace_prefix(Proof_id_str, Level), $>nicer_term(Query), Desc]); true),
	fail.

prove_body(Path, Proof_id_str, Ep_yield, Eps0, Ep_List, Query_ep_terms, Desc, Preps, Level, Body_items, Quiet, Proof) :-
	updated_ep_list(Eps0, Ep_List, Proof_id_str, Path, Query_ep_terms, Desc, Eps1),
	call_preps(Preps, Path),
	bump_step,
	body_proof(Path, Proof_id_str, Ep_yield, Level, Eps1, Body_items, Quiet, Proof).

call_preps(Preps, Path) :-
	maplist(call_prep(Path), Preps).

call_prep(Path, Prep) :-
	debug(pyco_prep, 'call prep: ~q', [Prep]),
	call(Prep, Path).

body_proof(Path, Proof_id_str, Ep_yield, Level, Eps1, Body_items, Quiet, Proof) :-
	body_proof2(Path, Proof_id_str, Ep_yield, Level, Eps1, Body_items, Quiet, Proof).

/* debugging, try again and print that it failed */
/*
body_proof(Path, Proof_id_str, Ep_yield, Level, Eps1, Body_items, Quiet, Proof) :-
	%(Quiet = noisy -> debug(pyco_proof, '~w disproving..', [$>trace_prefix(Proof_id_str, Level)]); true),
	\+body_proof2(Path, Proof_id_str, Ep_yield, Level, Eps1, Body_items, quiet, Proof),
	(Quiet = noisy -> debug(pyco_proof, '~w disproved.', [$>trace_prefix(Proof_id_str, Level)]); true),
	false.
*/
body_proof2(Path, Proof_id, Ep_yield, Level, Eps1, Body_items, Quiet, Proof) :-
	/*1. collect all delays at the beginning of rule body processing*/
	sorted_body_items_with_delays1(Body_items, Body_items2),
	body_proof3(Path, Proof_id, 0, Ep_yield, Level, Eps1, Body_items2, Quiet, Proof).

/* base case */
body_proof3(_Path, _Proof_id_str, _, _Ep_yield, _Level, _Eps1, [], _Quiet, []).


/*
2. body_proof3: either:
	a) grab 0 delay item
	b) re-collect delays of non-zero items, sort, grab first item.
*/


body_proof3(Path, Proof_id_str, Bi_idx, Ep_yield, Level, Eps1, Body_items, Quiet, [ProofH|ProofT]) :-
	(	append([Bis_head, [bi_with_delay(Body_item,0)], Bis_tail], Body_items)
	->	(	/* nice, we found a zero-delay body item */
			append(Bis_head, Bis_tail, Body_items2)
		)
	;	(
			sorted_body_items_with_delays2(Body_items, Body_items_sorted),
			Body_items_sorted = [bi_with_delay(Body_item,_) | Body_items2]
		)
	),
	ProofH = Body_item-Proof,
	append(Path, [bi(Bi_idx)], Bi_Path),
	proof(Bi_Path, Level, Eps1, Ep_yield, Quiet, Body_item, Proof),
	Bi_idx_next is Bi_idx + 1,
	body_proof3(Path, Proof_id_str, Bi_idx_next, Ep_yield, Level, Eps1, Body_items2, Quiet, ProofT).

bi_with_delay2(bi_with_delay(Bi,_), bi_with_delay(Bi,Delay)) :-
	!body_item_delay(Bi, Delay).

bi_with_delay1(Bi, bi_with_delay(Bi,Delay)) :-
	!body_item_delay(Bi, Delay).

sorted_body_items_with_delays1(Body_items, Body_items_sorted) :-
	maplist(bi_with_delay1, Body_items, Items),
	sort(2, @=<, Items, Body_items_sorted).

sorted_body_items_with_delays2(Body_items, Body_items_sorted) :-
	maplist(bi_with_delay2, Body_items, Items),
	sort(2, @=<, Items, Body_items_sorted).

body_item_delay(Bi, Delay) :-
	findall(Delay, delay(Bi, Delay), Delays),
	sum_list(Delays, Delay).




depth_map(X, v) :-
	var(X).

depth_map(X, Map) :-
	nonvar(X),
	X =.. [_|Args],
	maplist(depth_map, Args, Args2),
	Map =.. [nv|Args2].











/*
 ep stuff
*/

updated_ep_list(Eps0, Ep_List, Proof_id_str, Path, Query_ep_terms, Desc, Eps1) :-
	append(Ep_List, [ep(Proof_id_str, Path, Query_ep_terms)], Ep_List_New),
	Eps1 = Eps0.put(Desc, Ep_List_New).

print_debug_ep_list_item(I) :-
	debug(pyco_ep, '* ~q', [I]).

query_term_ep_terms(Query, Query_ep_terms) :-
	Query =.. [_|Args],
	maplist(arg_ep_table_term, Args, Query_ep_terms).

ep_list_for_rule(Eps0, Desc, X) :-
	(	get_dict(Desc, Eps0, X)
	->	true
	;	X = []).

ep_ok(Ep_List, Query_ep_terms, Quiet) :-
	%debug(pyco_ep, 'seen:~q', [Ep_List]),
	%debug(pyco_ep, 'now:~q ?', [Query_ep_terms]),
	maplist(ep_ok2(Query_ep_terms, Quiet), Ep_List),
	(Quiet = noisy -> debug(pyco_ep, 'ep_ok:~q', [Query_ep_terms]);true)
	.

ep_ok2(Query_ep_terms, Quiet, ep(Proof_id_str,Path,Ep_Entry)) :-
	length(Query_ep_terms, L0),
	length(Ep_Entry, L1),
	assertion(L0 == L1),
	findall(x,
		(
			between(1, L0, I),
			nth1(I, Ep_Entry, Old_arg),
			nth1(I, Query_ep_terms, New_arg),
			arg_is_productively_different(Quiet,Proof_id_str,Path, Old_arg, New_arg)
		),
		Differents),
	(	Differents == []
	->	(
			(Quiet = noisy -> debug(pyco_proof, 'EP!', []);true),
			false
		)
	;	true).


arg_ep_table_term(A, var) :-
	var(A).
arg_ep_table_term(A, const(A)) :-
	atomic(A).
arg_ep_table_term(A, bn(Uid_str, Tag)) :-
	nonvar(A),
	A = bn(Uid, Bn),
	is_dict(Bn, Tag),
	term_string(Uid, Uid_str).


%\+arg_is_productively_different(_,_, var, var).
arg_is_productively_different(_,_,_, var, const(_)).
arg_is_productively_different(_,_,_, var, bn(_,_)).
arg_is_productively_different(_,_,_, const(_), var).
arg_is_productively_different(_,_,_, const(C0), const(C1)) :- C0 \= C1.
arg_is_productively_different(_,_,_, const(_), bn(_,_)).
arg_is_productively_different(_,_,_, bn(_,_), var).
arg_is_productively_different(_,_,_, bn(_,_), const(_)).
arg_is_productively_different(Quiet,Proof_id_str,Path_where_old_bnode_was_seen, bn(Uid_old_str,Tag0), bn(Uid_new_str,Tag1)) :-
	assertion(string(Uid_old_str)),
	assertion(string(Uid_new_str)),
	/* differing types, success */
	(	Tag0 \= Tag1
	->	true
	;	(
			/* same uids, fail. */
			Uid_old_str \= Uid_new_str,
			(	(\+was_created_under(Quiet,Uid_new_str, Path_where_old_bnode_was_seen))
			->	true
			;	came_before(bn(Uid_new_str,_,_), fr(Proof_id_str)))
		)).

was_created_under(Quiet,Uid_new_str, Path_where_old_bnode_was_seen) :-
	b_getval(bn_log, Bn_log),
	member(bn(Uid_new_str, _, Path_where_new_bnode_was_created), Bn_log),
	prefix(Path_where_old_bnode_was_seen, Path_where_new_bnode_was_created),
	(Quiet = noisy -> debug(pyco_ep, 'old:~q', [Path_where_old_bnode_was_seen]);true),
	(Quiet = noisy -> debug(pyco_ep, 'new:~q', [Path_where_new_bnode_was_created]);true),
	true
	.

came_before(A, B) :-
	b_getval(bn_log, Bn_log),
	%debug(pyco_proof, 'came_before:~q', [nth0(Ib, Bn_log, B)]),
	assertion(nth0(Ib, Bn_log, B)),
	nth0(Ib, Bn_log, B),
	(	nth0(Ia, Bn_log, A)
	->	Ia < Ib
	;	true).

mkbn(Bn, Dict, Path) :-
	(
	/* avoid creating new bnode if we are already called with one. this eases tracking them for ep check purposes */
	(	nonvar(Bn),
		Bn = bn(_, Dict)
	)
	;
	(
		var(Bn),
		Bn = bn(_, Dict),
		register_bn(Bn, Path)
	)).

register_bn(bn(Uid, Dict), Path) :-
	is_dict(Dict, Tag),
	term_string(Uid, Uid_str),
	Entry = bn(Uid_str, Tag, Path),
	register_bn2(Entry).

register_frame(F) :-
	register_bn2(fr(F)).

register_bn2(Entry) :-
	b_getval(bn_log, Bn_log0),
	member(Entry, Bn_log0).

register_bn2(Entry) :-
	b_getval(bn_log, Bn_log0),
	\+ member(Entry, Bn_log0),
	append(Bn_log0, [Entry], Bn_log1),
	b_setval(bn_log, Bn_log1),
	debug(pyco_bn_log, 'bn_log:', []),
	maplist(debug_print_bn_log_item, Bn_log1).

debug_print_bn_log_item(I) :-
	debug(pyco_bn_log, '* ~q', [I]).

ep_debug_print_1(Ep_List, Query_ep_terms) :-
	debug(pyco_ep, 'seen:', []),
	maplist(print_debug_ep_list_item, Ep_List),
	debug(pyco_ep, 'now: ~q', [Query_ep_terms]).









/*
 calling prolog
*/

call_native(Proof_id_str, Level, Quiet, Query) :-
	/* this case tries to handle calling native prolog predicates */
	\+matching_rule(Level, Query, _,_,_,_,_),
	(Quiet = noisy -> debug(pyco_proof, '~w prolog call:~q', [$>trace_prefix(Proof_id_str, Level), Query]); true),
	call_native2(Proof_id_str, Level, Quiet, Query).

call_native2(Proof_id_str, Level, Quiet, Query) :-
	call_native3(Query),
	(Quiet = noisy -> debug(pyco_proof, '~w prolog call succeeded:~q', [$>trace_prefix(Proof_id_str, Level), Query]); true),
	true.

call_native2(Proof_id_str, Level, Quiet, Query) :-
	\+call_native3(Query),
	(Quiet = noisy -> debug(pyco_proof, '~w prolog call failed:~q', [$>trace_prefix(Proof_id_str, Level), Query]); true),
	fail.

call_native3(Query) :-
	% you'd think this would only catch when the Query term clause doesn't exist, but nope, it actually catches any nested exception. Another swipl bug?
	functor(Query, Name, Arity),
	catch(
		call(Query),
		error(existence_error(procedure,Name/Arity),_),
		fail
	).





/*
utils
*/

bump_step :-
	nb_getval(step, Step),
	Step_next is Step + 1,
	nb_setval(step, Step_next).

trace_prefix(Proof_id_str, Level, String) :-
	Level2 is Level + 64,
	char_code(Level_char, Level2),
	format(string(String), '~q ~w ~w', [$>nb_getval(step), Level_char, Proof_id_str]).







/*
runtime rule lookup
*/
matching_rule(_Level, Query, Id, Body_items, Preps, Query_ep_terms, Head_item_idx) :-
	query_term_ep_terms(Query, Query_ep_terms),
	pyco3_rule(Id, Head_items, Body_items, _Notes, _Cnls, _Names, Preps),
	nth0(Head_item_idx, Head_items, Query).


/*
rule check/printout
*/
collect_rules :-
	'make sure no rule declarations remained unexpanded',
	findall(
		Rule,
		(
			T = pyco3_rule(_,_,_,_,_,Names,_),
			call(T),
			print_term(T, [variable_names(Names)]),
			nl,nl,
	fail.


'make sure no rule declarations remained unexpanded' :-
	(
		(
				% see if there are any 'r' clauses to call. If there are not, we get an exception, catch it, all is well.
				catch(r(Bad),_,false),
				throw_string('p/1 declaration remained unexpanded'(Bad))
		);
		true
	).

