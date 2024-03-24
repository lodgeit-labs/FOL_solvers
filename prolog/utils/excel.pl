

/*
┏━╸╻ ╻┏━╸┏━╸╻
┣╸ ┏╋┛┃  ┣╸ ┃
┗━╸╹ ╹┗━╸┗━╸┗━╸
*/
 sheet_and_cell_string_for_property(Item, Prop, Str) :-
	!doc(Item, Prop, Value),
	!sheet_and_cell_string(Value, Str).

 sheet_and_cell_string(Value, Str) :-
	(	doc(Value, excel:has_sheet_name, Sheet_name)
	->	(	(doc(Value, excel:col, Col), doc(Value, excel:row, Row))
		->	!atomics_to_string(['sheet "', Sheet_name, '", cell ', Col, ':', Row], Str)
		;	!atomics_to_string(['sheet "', Sheet_name], Str))
	;	Str = "unknown location").

 read_coord_vector_from_doc_string(Item, Prop, Default_currency, Side, VectorA) :-
	doc_value(Item, Prop, Amount_string),
	(	vector_from_string(Default_currency, Side, Amount_string, VectorA)
	->	true
	;	throw_string(['error reading "amount" in ', $>!sheet_and_cell_string($>doc(Item, Prop))])).

 read_value_from_doc_string(Item, Prop, Default_currency, Value) :-
	doc_value(Item, Prop, Amount_string),
	(	value_from_string(Default_currency, Amount_string, Value)
	->	true
	;	(
			assertion(var(Value)),
			throw_string(['error reading "amount" in ', $>!sheet_and_cell_string($>doc(Item, Prop))])
		)
	).




 get_sheet(Type, Sheet) :-
	!doc($>request_data, excel:has_sheet_instances, Sheets),
	*doc_list_member(Sheet, Sheets),
	?doc(Sheet, excel:sheet_instance_has_sheet_type, Type).

 get_sheets(Type, Sheets) :-
 	findall(Sheet, get_sheet(Type, Sheet), Sheets).

 get_singleton_sheet(Type, Sheet) :-
 	get_sheets(Type, Sheets),
 	(	Sheets = [Sheet]
 	->	true
 	;	((	Sheets = []
 		->	throw_format('not expected: no sheets of type ~q', [Type])
 		;	true),
	 	throw_format('not expected: multiple sheets of type ~q', [Type]))).

 get_sheet_data(Type, Data) :-
	*get_sheet(Type, Sheet),
	!doc(Sheet, excel:sheet_instance_has_sheet_data, Data),
	!doc_add(Sheet, l:was_processed, true). % todo check.


 get_sheets_data(Type, Datas) :-
 	findall(Data, get_sheet_data(Type, Data), Datas).

 get_singleton_sheet_data(Type, Data) :-
 	get_sheets_data(Type, Datas),
 	(	Datas = [Data]
 	->	true
 	;	((	Datas = []
 		->	throw_format('not expected: no sheets of type ~q', [Type])
 		;	true),
	 	throw_format('not expected: multiple sheets of type ~q', [Type]))).

 get_optional_singleton_sheet_data(Type, Data) :-
 	get_sheets_data(Type, Datas),
 	length(Datas, L),
 	(	L #< 2
 	->	true
 	;	throw_format('not expected: multiple sheets of type ~q', [Type])),
 	?(Datas = [Data]).

 get_optional_singleton_sheet(Type, Sheet) :-
 	get_sheets(Type, Sheets),
 	length(Sheets, L),
 	(	L #< 2
 	->	true
 	;	throw_format('expected only one sheet of type ~q', [Type])),
 	Sheets = [Sheet].


% should we represent uris explicitly by wrapping them in uri()?


