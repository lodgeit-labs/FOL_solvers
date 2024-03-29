/*
	we need this constant, because float math operations generate different values after certain precision in different machines,
	also to compare results of essentially different computations that should lead to same results
*/

/* i dont think the concept of significant digits makes sense here */
/* we should probably be rounding to 6 digits and comparing to 0.1**5? */
/* output htmls should be given more structure and compared like xmls */

 float_comparison_significant_digits(4).

 float_comparison_max_difference(0.0001).
/*todo float_comparison_max_difference(X) :-
	float_comparison_significant_digits(D),
	X is 0.1**D, % is this represented correctly?*/

 floats_close_enough(Value1, Value2) :-
	float_comparison_max_difference(Max),
	ValueDifference is abs(Value1 - Value2),
	ValueDifference =< Max.

 round_to_significant_digit(X,Y) :-
	float_comparison_significant_digits(D),
	round(X, D, Y).

 round(X,_,X) :-
	integer(X),
	!.

 round(X,D,Y2) :-
	\+integer(X),
	(float(X);rational(X)),
	!,
	Z is X * 10^D,
	round(Z, ZA),
	Y is ZA / 10^D,
	Y2 is float(Y).


 terms_with_floats_close_enough(X,Y) :-
	(number(X);rational(X)),!,
	(number(Y);rational(Y)),!,
	floats_close_enough(X, Y).

 terms_with_floats_close_enough(X,Y) :-
	X =.. [Functor|Args1],
	Y =.. [Functor|Args2],
	maplist(terms_with_floats_close_enough, Args1, Args2).



 round_term(X, Y) :-
	float_comparison_significant_digits(D),
	round_term(D, X, Y).

 round_term(_, X, '[variable]') :-
	var(X),
	!.

 round_term(Digits, X, Y) :-
	(number(X);rational(X)),
	!,
	round(X, Digits, Y).

 round_term(Digits, X, Y) :-
	X =.. [Functor|Args],!,
	maplist(round_term(Digits), Args, Args2),
	Y =.. [Functor|Args2].


:- debug(numbers).

 unify_numbers(A,B) :-
	(	A = B
	->	true
	;	/* allow for integer vs float */
		(
			A =:= B,
			% ^ does this ever still happen?
			debug(numbers, '~w', [unify_numbers(A,B)])
		)
	).

/* todo unify_numbers(Z, 0)?*/
 is_zero_number(Z) :-
	var(Z), Z = 0.
 is_zero_number(Z) :-
	atomic(Z),
	Z =:= 0.


 is_numeric(X) :-
	number(X);rational(X),!.

 rat(X, Y) :-
	Y is rational(X).
