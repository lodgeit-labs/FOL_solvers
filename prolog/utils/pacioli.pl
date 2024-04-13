
/*

	coord and value

*/

:- record coord(unit, debit).
/* coord represents a debit or a credit in a ledger. Negative debit is a credit. */

:- record value(unit, amount).
/* value is used where the credit/debit distinction doesn't make sense or would be confusing. Just a number with a unit. */

/*

	convert between value and coord

*/

/*value_debit(value(Unit, Amount), coord(Unit, Amount, Zero)) :- unify_numbers(Zero, 0).
value_credit(value(Unit, Amount), coord(Unit, Zero, Amount)) :- unify_numbers(Zero, 0).*/

 coord_normal_side_value(coord(Unit, D), Side, value(Unit, D)) :-
 	e(Side, kb:debit).

 coord_normal_side_value(coord(Unit, D), Side, value(Unit, V)) :-
 	e(Side, kb:credit),
	{V = -D}.

 coord_normal_side_value2(Side, C, V) :-
	coord_normal_side_value(C, Side, V).


/*
	manipulating value and coord
*/

 coord_inverse(coord(Unit, A_Debit), coord(Unit, A_Credit)) :- {A_Credit = -A_Debit}.
 coord_inverse(value(Unit, Value), value(Unit, Value_Inverted)) :- {Value_Inverted = -Value}.

	
% -------------------------------------------------------------------
% Pacioli group operations. These operations operate on vectors. A vector is a list of
% coordinates. A coordinate is a triple comprising a unit, a debit amount, and a credit
% amount. See: On Double-Entry Bookkeeping: The Mathematical Treatment Also see: Tutorial
% on multiple currency accounting


% Computes the (additive) inverse of a given vector.
% - returns a vector of coordinates with debit and credit values switched around

 vec_inverse(As, Bs) :-
 	val(As, AsV),
	maplist(coord_inverse, AsV, BsV),
	doc_new_vec(BsV, Bs),
	doc_add(Bs, l:origin, As).

 vec_inverse_(As, Bs) :-
	maplist(coord_inverse, As, Bs).

/*
	reducing a coord to normal form is a no-op now that coords are represented with a single number,
	so all we do is remove zero coords.

*/

 vec_reduce_coords(As, Bs) :-
 	atom(As),var(Bs),
	exclude(is_zero_coord, As, Result_Nonzeroes),
	maplist(unify_coords_or_values, Bs, Result_Nonzeroes),
	! /*todo: is the cut needed?*/
	.


 coord_or_value_unit(coord(Unit,_), Unit).
 coord_or_value_unit(value(Unit,_), Unit).

 coord_or_value_amount(coord(_,A), A).
 coord_or_value_amount(value(_,A), A).



 vec_reduce_(X, Y) :-
	vec_add_(X, [], Y).



% Adds the two given vectors together and reduces coords or values in a vector to a minimal (normal) form.

 
 vec_reduce_coords_(As, Bs) :-
	exclude(is_zero_coord, As, Result_Nonzeroes),
	maplist(unify_coords_or_values, Bs, Result_Nonzeroes),
	! /*todo: is the cut needed?*/
	.

 vec_add(A,B,C) :-
	vec_sum([A,B], C).

% sum a list of vectors

 vec_sum(Vecs, Sum) :-
 	assertion(maplist(atom, Vecs)),
 	maplist(val, Vecs, Vectors),
 	flatten(Vectors, Flat),

	!sort_into_assoc_of_lists(!coord_or_value_unit, Flat, Assoc),
	!assoc_to_values(Assoc, Valueses),
	!maplist(semigroup_foldl(coord_or_value_merge), Valueses, Total),
	% Total_Flat is a list with one coord per each unittype in As and Bs combined
	flatten(Total, Total_Flat),
	exclude(is_zero_coord, Total_Flat, Result_Nonzeroes),
	
	doc_new_vec(Result_Nonzeroes, Sum),
	maplist(doc_add(Sum, l:part), Vecs).
	

 	
 


% Subtracts the vector Bs from As by inverting Bs and adding it to As.

 vec_sub(As, Bs, Cs) :-
	vec_add(As, $>vec_inverse(Bs), Cs).

% Checks two vectors for equality by subtracting the latter from the former and verifying
% that all the resulting coordinates are zero.

 vec_equality(As, Bs) :-
	vec_sub(As, Bs, Cs),
	assertion(Cs = []),
	forall(member(C, Cs), is_zero(C)).

 is_zero(Coord) :-
	is_zero_coord(Coord).
	
 is_zero(Value) :-
	is_zero_value(Value).

 is_zero(X) :-
 	atom(X),
 	val(X, [V]),
	is_zero(V).

 is_zero([X]) :-
	is_zero(X).

 is_zero_coord(coord(_, Zero1)) :- is_zero_number(Zero1).

 is_zero_value(value(_, Zero)) :-
	is_zero_coord(coord(_, Zero)).

 is_debit(coord(_, X)) :-
	X > 0. % todo: maybe this should be {}?

 is_debit([Coord]) :-
	is_debit(Coord).

 is_credit(coord(_, X)) :-
	X < 0. % todo: maybe this should be {}?

 is_credit([Coord]) :-
	is_credit(Coord).

 unify_coords_or_values(coord(U, D1), coord(U, D2)) :-
	unify_numbers(D1, D2).

 unify_coords_or_values(value(U, V1), value(U, V2)) :-
	unify_numbers(V1, V2).


 number_coord(Unit, Number, coord(Unit, Number)).
 credit_coord(Unit, Credit, coord(Unit, Number)) :- {Credit = -Number}.

 dr_cr_coord(Unit, Number, Zero, coord(Unit, Number)) :- {Number >= 0, Zero = 0}.
 dr_cr_coord(Unit, Zero, Credit, coord(Unit, Number)) :- {Number < 0, Zero = 0, Credit = -Number}.

 coord_of_vec(C, V) :-
 	atom(V),
 	val(V, [C]).

 coord_of_vec(coord(_U,0), V) :-
 	atom(V),
 	val(V, []).

 number_vec(_, Zero, []) :-
	unify_numbers(Zero, 0).
	
 number_vec(Unit, Number, [Coord]) :-
	number_coord(Unit, Number, Coord).

 credit_vec(Unit, Credit, [Coord]) :-
	assertion(var(Unit);atom(Unit)),
	credit_coord(Unit, Credit, Coord).


 debit_isomorphism(Coord, C) :-
	number_coord(_, C, Coord).

 coord_or_value_of_same_unit(A, B) :-
	coord_unit(A, A_Unit),
	coord_unit(B, A_Unit).
 coord_or_value_of_same_unit(A, B) :-
	value_unit(A, A_Unit),
	value_unit(B, A_Unit).

 coord_or_value_merge(coord(Unit, D1), coord(Unit, D2), coord(Unit, D3)) :-
	{D3 = D2 + D1}.
	
 coord_or_value_merge(value(Unit, D1), value(Unit, D2), value(Unit, D3)) :-
	{D3 = D2 + D1}.

 coord_merge(coord(Unit, D1), coord(Unit, D2), coord(Unit, D3)) :-
	{D3 = D2 + D1}.

 value_merge(value(Unit, D1), value(Unit, D2), value(Unit, D3)) :-
	{D3 = D2 + D1}.

 value_convert(value(Unit, Amount1), exchange_rate(_,Src,Dst,Rate), value(Unit2, Amount2)) :-
	assertion(Unit = Src),
	assertion(Unit2 = Dst),
	Unit2 = Dst,
	{Amount2 = Amount1 * Rate}.
	
 value_multiply(value(Unit, Amount1), Multiplier, value(Unit, Amount2)) :-
	{Amount2 = Amount1 * Multiplier}.

 value_divide(value(Unit, Amount1), Divisor, value(Unit, Amount2)) :-
	{Amount2 = Amount1 / Divisor}.

 value_divide2(value(U1, A1), value(U2, A2), exchange_rate(xxx, U2, U1, Rate)) :-
	{A1 / A2 = Rate}.

 value_subtract(value(Unit1, Amount1), value(Unit2, Amount2), value(Unit2, Amount3)) :-
	assertion(Unit1 == Unit2),
	{Amount3 = Amount1 - Amount2}.
	
 vecs_are_almost_equal(A, B) :-
	vec_sub(A, B, C),
	maplist(coord_is_almost_zero, C).

 coord_is_almost_zero(coord(_, D)) :-
	floats_close_enough(D, 0).

 coord_is_almost_zero(value(_, V)) :-
	floats_close_enough(V, 0).

 vector_of_coords_vs_vector_of_values(Side, Coords, Values) :-
 	(
 		(is_list(Coords),var(Values))
 		;
 		(var(Coords),is_list(Values))
 	),
	maplist(coord_normal_side_value2(Side), Coords, Values).

 vector_of_coords_vs_vector_of_values(Side, Coords, Values) :-
	atom(Coords),
	var(Values),
	val(Coords, CoordsV),
	maplist(coord_normal_side_value2(Side), CoordsV, ValuesV),
	doc_new_vec(ValuesV, Values),
	doc_add(Values, l:origin, Coords).

 vector_of_coords_vs_vector_of_values(Side, Coords, Values) :-
	var(Coords),
	atom(Values),
	val(Values, ValuesV),
	maplist(coord_normal_side_value2(Side), CoordsV, ValuesV),
	doc_new_vec(CoordsV, Coords),
	doc_add(Coords, l:origin, Values).

 split_vector_by_percent(V0, Rate, V1, V2) :-
	maplist(split_coord_by_percent(Rate), V0, V1, V2).

 split_coord_by_percent(Rate, H0, H1, H2) :-
	H0 = coord(U, D0),
	{D1 = D0 * Rate / 100},
	{D2 = D0 - D1},
	H1 = coord(U, D1),
	H2 = coord(U, D2).

 split_coord_by_percent(Rate, H0, H1, H2) :-
	H0 = value(U, D0),
	{D1 = D0 * Rate / 100},
	{D2 = D0 - D1},
	H1 = value(U, D1),
	H2 = value(U, D2).



 vector_unit(V, U) :-
 	val(V, [coord(U, _)]).
 	
 	
 	
 value_debit_vec(Value, [Coord]) :-
	coord_normal_side_value(Coord, kb:debit, Value).

 value_debit_vec(value(_,Z), []) :-
	is_zero_number(Z).

 value_credit_vec(Value, [Coord]) :-
	coord_normal_side_value(Coord, kb:credit, Value).


 vec_is_almost_zero(Vec) :-
 	!atom(Vec),
 	val(Vec, VecV),
 	!is_list(VecV), %see report_entry_total_vec
	maplist(coord_is_almost_zero, VecV).


 vec_add_(As, Bs, Cs_Reduced) :-
	%cd('ensure As and Bs are flat lists', assertion((flatten(As, As), flatten(Bs, Bs)))),
	!append(As, Bs, As_And_Bs),
	!sort_into_assoc_of_lists(!coord_or_value_unit, As_And_Bs, Assoc),
	!assoc_to_values(Assoc, Valueses),
	!maplist(semigroup_foldl(coord_or_value_merge), Valueses, Total),
	% Total_Flat is a list with one coord per each unittype in As and Bs combined
	flatten(Total, Total_Flat),
	!vec_reduce_coords_(Total_Flat, Cs_Reduced).

 coord_vec_(coord(U,A), [coord(U,A)]).
 coord_vec_(coord(_U,0), []).
