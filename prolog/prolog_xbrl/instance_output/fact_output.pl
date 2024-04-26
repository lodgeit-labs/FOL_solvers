
/*
coord(
	Unit						% atom:Unit
	Debit						% Numeric
).
*/

/*
entry(
	Name						% atom:Entry Name
	Balances					% List ...
	Children					% List entry
	?							% 
).
*/





/*
pesseract_style_table_rows(Report_Uri,
	Report_Currency,			% List atom:Report Currency
	Entries,					% List entry
	[Lines|Lines_Tail]			% List (List line)
) 
*/	

 pesseract_style_table_rows(_Report_Uri, _Report_Currency, [], []).
 pesseract_style_table_rows(Report_Uri,
	Report_Currency,
	Entries,
	[Lines|Lines_Tail]
) :-
	[Entry|Entries_Tail] = Entries,
	!doc_add(Report_Uri, report_entries:entry, Entry),

	!report_entry_name(Entry, Name),
	!report_entry_normal_side(Entry, Normal_Side),
	!report_entry_total_vec(Entry, Balance),
	!report_entry_children(Entry, Children),

	/*render child entries*/
	!pesseract_style_table_rows(Report_Uri, Report_Currency, Children, Children_Rows),
	/*render balance*/
	!format_vec(html, Report_Currency, _, Name, Normal_Side, Balance, Balance_Lines),
	(	Children_Rows = []
	->	!entry_row_childless(Name, Balance_Lines, Entry, Lines)
	;	!entry_row_childful(Name, Entry, Children_Rows, Balance_Lines, Lines)),
	/*recurse on Entries_Tail*/
	!pesseract_style_table_rows(Report_Uri, Report_Currency, Entries_Tail, Lines_Tail).

entry_row_childless(Name, Balance_Lines, Entry, Lines) :-
	!entry_row(cols{0:Name,1:Balance_Lines}, Entry, report_entries:single, Lines).

entry_row_childful(Name, Entry, Children_Rows, Balance_Lines, Lines) :-
	Lines =
	[
		$>!entry_row(cols{0:b([Name])}, Entry, report_entries:header),
		Children_Rows,
		$>!entry_row(cols{0:td([align="right"],[Name]),1:Balance_Lines}, Entry, report_entries:footer)
	].

miscs_dict(Entry, Type, Dict) :-
	findall(
		Col_Pos-Item,
		(
			between(1,5,C),
			Col_Pos is C + 1,
			entry_misc_item_for_column(Entry, Type, C, Item)
		),
		Pairs),
	dict_pairs(Dict, cols, Pairs).

entry_misc_item_for_column(Entry, Type, Column, Item) :-
	doc(Entry, report_entries:misc, D1),
	doc(D1, report_entries:column, Column),
	doc(D1, report_entries:misc_type, $>rdf_global_id(Type)),
	doc(D1, report_entries:value, Item).

entry_row(Cols0, Entry, Type, Row) :-
	miscs_dict(Entry, Type, Miscs_Dict),
	merge_dicts(Cols0, Miscs_Dict, Cols),
	cols_dict_to_row(Cols, Row).

cols_dict_to_row(Cols, tr(Tds)) :-
	dict_pairs(Cols, _, Pairs),
	findall(I,cols_dict_to_row_helper(Pairs, I), Tds).

cols_dict_to_row_helper(Pairs, I) :-
	between(0,6,C),
	cols_dict_to_row_helper2(C, Pairs, I).

cols_dict_to_row_helper2(C, Pairs, I) :-
	member(C-Item, Pairs),
	cols_dict_to_row_helper3(Item, I).

cols_dict_to_row_helper2(C, Pairs, td([])) :-
	\+member(C-_, Pairs),
	there_is_item_after(C, Pairs).

cols_dict_to_row_helper3(Item, I) :-
	(	Item =.. [td|_]
	->	I = Item
	;	(
			I = td(Item2),
			flatten([Item], Item2)
		)).



 there_is_item_after(C, Pairs) :-
	findall(X,
		(
			member(C2-X, Pairs),
			C2 > C
		),
		Items),
	Items \= [].






/*
	Format,					% atom:{html,xbrl}
	Report_Currency_List,	% List atom:Report Currency
	Context,				%
	Name,					% atom:Entry Name
	Normal_Side,			% atom:{credit,debit}
	Coord,					% Vector coord {0,1}
	Xml						% ...
).
*/

 format_vec(Format, Report_Currency_List, Context, Name, Normal_Side, Vec, Out) :-
	!atom(Vec),
	!val(Vec, V),
	(	V = []
	->	
		(
			% just for displaying zero balance when the balance vector is []),
			(	[Report_Currency] = Report_Currency_List
			->	true
			;	Report_Currency = ''),
			!format_coord(Format, _, Context, Name, Normal_Side, coord(Report_Currency, 0), Vec_str)
		)
	;
		(
			!maplist(format_coord(Format, Report_Currency_List, Context, Name, Normal_Side), V, Coord_strs),
			atomics_to_string(Coord_strs, Vec_str)
		)
	),
	
	(	format = html
	->	(
			Out = span(
				$>append(
					[Vec_str],
					[$>link(Vec)]))
		)
	;	Out = Vec_str).
					

	
  
format_coord(Format, Report_Currency_List, Context, Name, Normal_Side, coord(Unit, Debit), Line) :-

	(	e(Normal_Side, 'https://rdf.lodgeit.net.au/v1/kb#credit')
	->	Balance0 is -Debit
	;	(
			e(Normal_Side, 'https://rdf.lodgeit.net.au/v1/kb#debit'),
			Balance0 is Debit
	)),

	round(Balance0, 2, Balance1),

	(	Balance1 =:= 0
	->	Balance = 0 % get rid of negative zero
	;	Balance = Balance0),

	(	Format = xbrl
	->
		format(string(Amount), '~2:f', [Balance]),

		atomic_list_concat(['basic:', Name], Fact_Name), % fixme
		sane_unit_id(Unit, Sane_Unit_Id),
		Line = element(Fact_Name,
		[
			contextRef=Context,
			unitRef=Sane_Unit_Id,
			decimals="INF"],
			[Amount]),
		result_add_property(l:used_unit, Unit, xml)
	;
		(	Format = html
		->	(
				(	Report_Currency_List = [Unit]
				->	Printed_Unit = ''
				;	round_term(Unit, Printed_Unit)),
				format(string(Line), '~2:f~w\n', [Balance, Printed_Unit])
			)
		;	(
				round_term(Unit, Printed_Unit)),
				format(string(Line), '~5:f~w', [Balance, Printed_Unit])
			)
	).

sane_unit_id(Unit, Id) :-
	sane_id(Unit, 'U-', Id).



report_currency_atom(Report_Currency_List, Report_Currency_Atom) :-
	(
		Report_Currency_List = [Report_Currency]
	->
		atomic_list_concat(['(', Report_Currency, ')'], Report_Currency_Atom)
	;
		Report_Currency_Atom = ''
	).
