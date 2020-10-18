
fetch_triples(Graph) :-
	%writeln('got triples:\n====='),
	findall((S,P,O),
		(
			(l ?? rdf(S, P, O, Graph)),
			%writeq((S, P, O)),	nl,
   			rdf_assert(S, P, O)
        ),
	_Triples),
	%writeln('====\n===='),
	%writeq(Triples),
	%nl
	.
