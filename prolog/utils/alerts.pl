
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


