:- ['../utils.pl'].

test0 :-
	findall(
		_,
		(
			between(0, 2000000, Abs_Day),
			(test0a(Abs_Day) -> true ; format('Failed at ~w~n', [Abs_Day]))
		),
		_
	).
	
test0a(Abs_Day) :-
	!absolute_day(Date, Abs_Day),

	Abs_DayN is Abs_Day + 1,
	
	!gregorian_date(Abs_Day, Date),
	!gregorian_date(Abs_DayN, DateN),

	Date @< DateN,
	
	absolute_day(DateN, Abs_DayN2),
	absolute_day(Date, Abs_Day2),

	Abs_DayN2 = Abs_DayN,
	Abs_Day2 = Abs_Day,

	% once more with bound vars
	absolute_day(DateN, Abs_DayN2),
	absolute_day(Date, Abs_Day2).




 test1 :-
 	findall(
	 	_,
		(
			*d(J1985,Y,M,D),
			J #= J1985 + 724642,
			!test1a(J, date(Y,M,D))
		),
		_).

test1a(J, Date) :-

	% try with unbound argument
	!rata_die_to_gregorian_date(J, Date0),
	!(Date0 = Date),
	% try with bound argument
	!rata_die_to_gregorian_date(J, Date0),

	% try with unbound argument
	!date_to_absolute_day0(Date0, Day0),
	!(Day0 = J),
	% try with bound argument
	!date_to_absolute_day0(Date0, Day0).


:- initialization(run).

run :-
	debug,
	%load_files('helper/days_python_enumerated_comparison.pl'),
	%format('days_python_enumerated_comparison.pl loaded~n', []),
	test1,
	format('success', []).
