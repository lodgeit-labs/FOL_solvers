
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



 result_accounts(As) :-
	result(D),
	!doc(D, l:account_hierarchy, As).

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
 	rp(l:result_data_uri_base, B).




 add_result_sheets_report(Graph) :-
	save_doc_graph_as_report(Graph, result_sheets).


