:- ['../utils.pl'].

run :-
	!doc_init,
	
	!doc_add(xxx, rdf:type, l:'Request'),
	!doc_add(xxx, rdf:type, l:'Request1'),
	!doc_add(xxx, rdf:type, l:'Request2'),
	!doc_add(xxx, rdf:type, l:'Request3'),
	!doc_add(xxx, rdf:type, l:'Request4'),
	!doc_add(xxx, rdf:type, l:'Request5'),
	
	findall((S,P,O,G),
		 doc(S,P,O,G),
		 X1s),
	writeq(X1s),nl,nl,
	
	
	!doc_add(xxxyyy, rrrtsrte, "srtrstsr"),
	gtrace,
	!doc_add(xxxzzz, strstsrt, _),
	
	findall((S,P,O,G),
		 doc(S,P,O,G),
		 X2s),
	writeq(X2s),nl,nl,
	
	
	
	!doc_add(_, rrrtsrte, "srtrstsrvvvvvv"),
	!doc_add(_, strstsrt, 55465465),
	
	findall((S,P,O,G),
		 doc(S,P,O,G),
		 X3s),
	writeq(X3s),nl,nl,
	
	
	
	!doc(zxctsdtst, rrrtsrte, "srtrstsrvvvvvv"),
	
	
	findall((S,P,O,G),
		 doc(_, strstsrt, 55465465),
		 X4s),
	writeq(X4s),nl,nl,
	
	
	
	true.
	
	
	
