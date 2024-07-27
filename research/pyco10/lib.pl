% q with graph subsumption
%containing graph declares subsumption of subgraph.

q(S,P,O,G) :-
  q(S,P,O,X),
  q(X,is_part_of,G,G).





pyco0_rule(
	'preprocess',
	[preprocess(Verbs, S_transactions, Transactions)]
	<=
	[
		member(Verb, Verbs),
		produces(Verb, St, Ts),
		slice_out_a_list(Transactions, Ts, Transactions_rest),
		fr(S_transactions, St, S_transactions_tail),
		preprocess(Verbs, S_transactions_tail, Transactions_rest)
	]).

this should be done like this:


's_transactions to transactions(Verbs, S_transactions, Transactions) :-
	maplist(produces(Verbs), Sts, Tss),
	append(Tss, Transactions).
	
's_transactions to transactions(Verbs, S_transactions, Transactions) :-
	maplist(produces(Verbs), Sts, Tss),
	append(Tss, Transactions).
	










