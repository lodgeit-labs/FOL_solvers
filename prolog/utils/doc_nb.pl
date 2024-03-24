
 doc_add(S,P,O) :-
	% as an additional optimization diverging from doc_b, we'll hardcode default graph name:
	doc_add(S,P,O,default).


%
%doc_add(S,P,O,G) :-
%	/* is there a common pattern where expansion is not needed? */
%	rdf_global_id(S, S2),
%	rdf_global_id(P, P2),
%	rdf_global_id(O, O2),
%	rdf_global_id(G, G2),
%	doc_trace0(doc_add(S2,P2,O2,G2)),
%	%debug(doc, 'add:~q~n', [(S2,P2,O2,G2)]),
%	addd(S2,P2,O2,G2).
%





% big todo: can we achieve deterministic ordering of triples?
/* or should user code handle this? where is the offending code? */





/* should we ifdef t/2 for speed? */

