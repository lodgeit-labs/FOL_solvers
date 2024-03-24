
 doc_add(S,P,O) :-
  	%assertion(ground((S,P,O))),
	doc_default_graph(G),
	doc_add(S,P,O,G).

