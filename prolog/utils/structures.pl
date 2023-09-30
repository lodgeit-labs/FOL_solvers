
 merge_dicts(D1, D2, D3) :-
	dict_pairs(D1, Tag, P1),
	dict_pairs(D2, Tag, P2),
	append(P1, P2, P3),
	dict_pairs(D3, Tag, P3).

/*
	given a list of terms, for example, of transactions, and a Selector_Predicate, for example transaction_account,
	produce a dict with keys returned by the selector, and values lists of transaction terms.

	should be O(n)-ish.
*/
 sort_into_dict(Selector_Predicate, Ts, D) :-
	sort_into_dict(Selector_Predicate, Ts, _{}, D).

:- meta_predicate sort_into_dict(2, ?, ?, ?).

 sort_into_dict(Selector_Predicate, [T|Ts], D, D_Out) :-
	call(Selector_Predicate, T, A),
	(
		L = D.get(A)
	->
		true
	;
		L = []
	),
	append(L, [T], L2),
	D2 = D.put(A, L2),
	sort_into_dict(Selector_Predicate, Ts, D2, D_Out).

 sort_into_dict(_, [], D, D).



/* same as sort_into_dict, but Selector_Predicate is ternary, getting an item of the supplied list, and returning a key and a value. should we merge these ? */
 sort_into_dict2(Selector_Predicate, Ts, D) :-
	sort_into_dict2(Selector_Predicate, Ts, _{}, D).

:- meta_predicate sort_into_dict2(3, ?, ?, ?).

 sort_into_dict2(Selector_Predicate, [T|Ts], D, D_Out) :-
	call(Selector_Predicate, T, A, V),
	(	L = D.get(A)
	->	true
	;	L = []),
	append(L, [V], L2),
	D2 = D.put(A, L2),
	sort_into_dict2(Selector_Predicate, Ts, D2, D_Out).

 sort_into_dict2(_, [], D, D).



/*
sort_into_dict_on_success/3(
	P,			% pred(Item,Key)
	Input,		% List Item
	Output		% Dict Item = {Key:[Value | Value in Input, P(Value,Key)] | Value in Input, P(Value, Key)}

).
*/
/* like sort_into_dict, but keep going if the predicate fails */
 sort_into_dict_on_success(P, Input, Output) :-
	sort_into_dict_on_success(P, Input, _{}, Output).


/*
sort_into_dict_on_success/4(
	P,			% pred(Item,Key)
	Input,		% List Item
	Current,	% Dict Item (accumulator)
	Output		% Dict Item
).

*/
 sort_into_dict_on_success(_, [], Output, Output).
 sort_into_dict_on_success(P, [I|Is], D, Output) :-
	(
		% should probably be wrapped in try/catch since sometimes it fails by error % mm i'd let that propagate
		call(P,I,Key)
	->
		New_Value = [Key-[I | D.Key]],
		dict_pairs(New_Key_Value, _, New_Value),
		Next_D = D.put(New_Key_Value)
	;	Next_D = D
	),
	sort_into_dict_on_success(P, Is, Next_D, Output).



 sort_into_assoc_of_lists(Selector_Predicate, Ts, D) :-
	empty_assoc(A),
	sort_into_assoc_of_lists(Selector_Predicate, Ts, A, D).

:- meta_predicate sort_into_assoc_of_lists(2, ?, ?, ?).

 sort_into_assoc_of_lists(Selector_Predicate, [T|Ts], D, D_Out) :-
	call(Selector_Predicate, T, A),
	(
		get_assoc(A, D, L)
	->
		true
	;
		L = []
	),
	append(L, [T], L2),
	put_assoc(A, D, L2, D2),
	sort_into_assoc_of_lists(Selector_Predicate, Ts, D2, D_Out).

 sort_into_assoc_of_lists(_, [], D, D).



:- meta_predicate find_thing_in_tree(?, 2, 3, ?).

 find_thing_in_tree(Root, Matcher, _, Root) :-
	\+is_list(Root),
	call(Matcher, Root).

 find_thing_in_tree([Entry|_], Matcher, Children_Yielder, Thing) :-
	find_thing_in_tree(Entry, Matcher, Children_Yielder, Thing).

 find_thing_in_tree([_|Entries], Matcher, Children_Yielder, Thing) :-
	find_thing_in_tree(Entries, Matcher, Children_Yielder, Thing).

 find_thing_in_tree(Root, Matcher, Children_Yielder, Thing) :-
	\+is_list(Root),
	call(Children_Yielder, Root, Child),
	find_thing_in_tree(Child, Matcher, Children_Yielder, Thing).



 path_get_dict((X/Y), Dict, Y_Value) :-
	path_get_dict(X, Dict, X_Value),
	path_get_dict(Y, X_Value, Y_Value).

 path_get_dict(K, Dict, V) :-
	K \= (_/_),
	get_dict(K, Dict, V).



 unzip([], [], []).
 unzip([X,Y|T], [X|XT], [Y|YT]) :-
	unzip(T, XT, YT).


 remove_before(Slash, Name_In, Name_Out) :-
   once((
   memberchk(Slash, Name_In)
   ->
     reverse(Name_In, RName),
     append(RFName, [Slash|_R1], RName),
     reverse(RFName, Name_Out)
   ;
     Name_Out = Name_In)
    ).


 add(Open_List, Item) :-
	once((
		member(M, Open_List),
		var(M),
		Item = M)).

/* produce L with first min(N,length(Src)) elements from Src, */
 take(Src, N, L) :-
   when(ground(N+Src), findall(E, (nth1(I,Src,E), I =< N), L)).


/* member/2 with reversed arguments */
 rebmem(Hay, Needle) :-
	member(Needle, Hay).


 assert_ground(X) :-
	assertion(ground(X)).




 sort_pairs_into_dict(Pairs, Dict) :-
	!sort_into_dict2([(A, B), A, B]>>true, Pairs, Dict).



 pair_to_json(Pair, Dict) :-
 	Pair =.. [_,K,V],
 	Dict0 = j{},
 	Dict = Dict0.put(K,V).
