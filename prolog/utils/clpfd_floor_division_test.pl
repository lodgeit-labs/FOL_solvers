:- use_module(library(clpfd)).

clpfd_floor_division_test :-
	findall(
		_,
		(
			between(-10, 10, X),
			between(-10, 10, Y),
			Z #= X // Y,
			Zflipped #= Z * -1,


			Xflipped #= X * -1,
			Zflipped1 #= Xflipped // Y,

			Yflipped #= Y * -1,
			Zflipped2 #= X // Yflipped,


			(	Zflipped = Zflipped1, Zflipped = Zflipped2
			->	true
			;	(
					format('X: ~w, Y: ~w, Z: ~w, Zflipped1: ~w, Zflipped2: ~w~n', [X, Y, Z, Zflipped1, Zflipped2]),
					throw('clpfd_floor_division_test failed')
				)
			)
		),
		_
	).
:- clpfd_floor_division_test.