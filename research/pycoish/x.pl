/*
A sheet might be specially marked to support a more direct way to interface with the model that the reasoner uses. Every record now supports a special "meta" column, and its contents are interpreted this way:

no contents: the record is a fact
"?": the record is a question. Any number of cells can contain a question mark, and the reasoner will try to fill them in.
"..." on the last row, with all the other cells empty: the list of records is open-ended. This is useful when the number of records is not known in advance. The reasoner will try to fill in zero or more missing records.

The resulting model will be displayed in a new sheet. An option of overwriting the original sheet is possible, for example to retain side notes.

Now, this "new" method corresponds more closely to the original idea of "doc", and in this way, the code does not have to explicitly create and structure "output" documents. This saves a whole lot of additional "doc_add" calls, and helps keep things declarative.

*/

process_request(basic_accounting_equation) :-
	doc(report1, assets, A),
	doc(report1, liabilities, L),
	doc(report1, equity, E),

	{A - L = E}.



%=============



process_request(basic_smsf_accounting_equation) :-
	doc(report1, assets, A),
	doc(report1, liabilities, L),
	doc(report1, equity, E),
	sum(Members) = E,

	{A - L = E}.



%=============



process_request(basic_smsf_accounting_equation) :-
	doc(report1, assets, A),
	doc(report1, liabilities, L),
	doc(report1, equity, E),
	sum(Members, E),

	{A - L = E}.

sum([], 0).
sum(H|T], S) :-
	sum(T, ST),
	{S = ST + H}.


