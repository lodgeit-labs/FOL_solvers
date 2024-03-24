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


