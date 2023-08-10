:- ['../utils.pl'].

run :-
	findall(
		_,
		(
			between(0, 2000000, Abs_Day),
			(test(Abs_Day) -> true ; format('Failed at ~w~n', [Abs_Day]))		
		),
		_
	).
	
test(Abs_Day) :-
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



d(Day, Date) :- 
	findall(Date0, gregorian_date(Day, Date0), [Date]),
	gregorian_date(Day, Date),
	findall(Day0, absolute_day(Date, Day0), [Day]),
	absolute_day(Date, Day).



:- ['helper/days_python_enumerated_comparison.pl'].
:- format('days_python_enumerated_comparison.pl loaded~n', []).
:- run.
:- format('success', []).
