
% not used.
% vec_units_(Vec, Units) :-
%	findall(Unit,
%	(
%		member(X, Vec),
%		coord_or_value_unit(X, Unit)
%	),
%	Units0),
%	sort(Units0, Units).

% not used.
% _vec_filtered_by_unit(Vec, Unit, Filtered) :-
%	findall(Coord,
%	(
%		coord_or_value_unit(Coord, Unit),
%		member(Coord, Vec)
%	),
%	Filtered).

% faster, but will not work while units are compound terms.
% not used.
% vec_add(A, B, C) :-
% 	vec_add2_(A, vec{}, Dict2),
% 	vec_add2_(B, Dict2, Dict3),
% 	assoc_to_values(Dict3, Coords),
%	!vec_reduce_coords(Coords, C).
%
% vec_add2_([], Dict, Dict).
%
% vec_add2_([coord(U,A1)|Coords], Dict, Dict_out) :-
% 	(	get_dict(U, Dict, coord(U, A2))
% 	->	(
% 			{A1 + A2 = A3},
% 			Dict2 = Dict.put(U, coord(U, A3))
% 		)
% 	;	Dict2 = Dict.put(U, coord(U, A1))),
% 	vec_add2_(Coords, Dict2, Dict_out).
%
% vec_add2_([value(U,A1)|Coords], Dict, Dict_out) :-
% 	(	get_dict(U, Dict, value(U, A2))
% 	->	(
% 			{A1 + A2 = A3},
% 			Dict2 = Dict.put(U, value(U, A3))
% 		)
% 	;	Dict2 = Dict.put(U, value(U, A1))),
% 	vec_add2_(Coords, Dict2, Dict_out).
%

% not used.
% vec_sum_(Vectors, Sum) :-
%	foldl(vec_add_, Vectors, [], Sum).

/*
sum_by_pred(
	P,			% pred(Item, Numeric)
	Input,		% List<Item>
	Sum			% Numeric = sum {X | Item in Input, P(Item,X)}
).
*/
% sum_by_pred(P, Input, Sum) :-
%	convlist(P, Input, Intermediate),
%	sumlist(Intermediate, Sum).

/*
vec_sum_by_pred(
	P,			% pred(Item, List record:coord)
	Input,		% List Item
	Sum			% List record:coord = vec_sum {X | Item in Input, P(Item, X)}
).
*/
% vec_sum_by_pred(P, Input, Sum) :-
%	convlist(P, Input, Intermediate),
%	vec_sum(Intermediate, Sum).


 

 credit_isomorphism(Coord, C) :-
	number_coord(_, D, Coord),
	{C = -D}.



% vec_sum(Vecs, Sum) :-
% 	assertion(maplist(atom, Vecs)),
% 	maplist(val, Vecs, Vectors),
%	foldl(vec_add_, Vectors, [], Sum_),
%	doc_new_vec(Sum_, Sum),
%	%doc_add(Sum, l:source, Vecs),
%	maplist(doc_add(Sum, l:part), Vecs).

% sum_of_vec_of_same_units([], []).
% sum_of_vec_of_same_units([coord(U,V)|T], [coord(U,Sum)]) :-
% 	sum_of_vec_of_same_units(T, [coord(U,Sum_T)]),
% 	Sum is V + Sum_T.


% The identity for vector addition.

 vec_identity_([]).



 vector_unit_([coord(U, _)], U).

