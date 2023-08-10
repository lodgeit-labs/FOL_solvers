:- use_module(library(clpfd)).

% -------------------------------------------------------------------
% The purpose of the following program is to define modular dates, a generalization of
% Gregorian dates where the day and month can take on any integral value. They allow you
% to do things like specify a date relative to the end of a month, add a month to it and
% get a date relative to the end of the next month. The program also defines absolute
% days, the number of days since since 1st January 0001. And accordingly it provides
% relations to convert from modular dates to absolute days.
%
% This program is a part of a larger system for deriving various pieces of information on
% different financial arrangements, hire purchase arrangements being an example. This
% larger system uses absolute days to represent time internally because they are easier
% to manipulate with than Gregorian dates.




 sensible_date(date(Year, Month, Day)) :-
	% not entirely comfortable with making this put hooks on a variable and then have unrelated code fail mysteriously,
	% so, let's limit it to checking ground terms for now
	!ground(date(Year, Month, Day)),
 	(	Day #< 32
 	->	true
 	;	throw_format('bad day: ~q', [Day])),
 	(	Day #> 0
 	->	true
 	;	throw_format('bad day: ~q', [Day])),
 	(	Month #< 13
 	->	true
 	;	throw_format('bad month: ~q', [Day])),
 	(	Month #> 0
 	->	true
 	;	throw_format('bad month: ~q', [Day])),
 	(	Year #< 2035
 	->	true
 	;	throw_format('suspicious year: ~q', [Day])),
 	(	Year #> 1985
 	->	true
 	;	throw_format('suspicious year: ~q', [Day])).




%
% Some facts about the Gregorian calendar, needed to count days between dates
%---------------------------------------------------------------------
 /* The year must be evenly divisible by 4;
 If the year can also be evenly divided by 100, it is not a leap year;
 unless...The year is also evenly divisible by 400. Then it is a leap year.*/
 leap_year(Year) :-
 		0 is mod(Year, 4),
 		X is mod(Year, 100),
 		X =\= 0.

 leap_year(Year) :- 0 is mod(Year, 400).

/* common year should be the inverse of leap_year, which is presumably checked in various places thanks to determinancy checker */
 common_year(Year) :-
	((Y is mod(Year, 4), Y =\= 0); 0 is mod(Year, 100)),
	Z is mod(Year, 400), Z =\= 0.



 days_in(_, 1, 31).
 days_in(Year, 2, 29) :- leap_year(Year).
 days_in(Year, 2, 28) :- common_year(Year).
 days_in(_, 3, 31).
 days_in(_, 4, 30).
 days_in(_, 5, 31).
 days_in(_, 6, 30).
 days_in(_, 7, 31).
 days_in(_, 8, 31).
 days_in(_, 9, 30).
 days_in(_, 10, 31).
 days_in(_, 11, 30).
 days_in(_, 12, 31).

 days_in(Year, Month, Days) :-
 	Month =< 0,
 	Closer_Year is Year - 1,
 	Closer_Year_Month is 12 + Month,
 	days_in(Closer_Year, Closer_Year_Month, Days).

 days_in(Year, Month, Days) :-
 	Month > 12,
 	Closer_Year is Year + 1,
 	Closer_Year_Month is Month - 12,
 	days_in(Closer_Year, Closer_Year_Month, Days).


 % -------------------------------------------------------------------
 % A generalized date, date(Y, M, D), means the same thing as a normal date when its value
 % is that of a normal date. In addition date(2006, 0, 1) refers to the month before
 % date(2006, 1, 1), date(2006, -1, 1) to the month before that, etc. Also date(2006, 5, 0)
 % refers to the day before date(2006, 5, 1), date(2006, 5, -1) to the day before that,
 % etc. Months have precedence over days. Useful for specifying that payments happen at
 % month-ends.
 % -------------------------------------------------------------------

 date_add(date(A, B, C), date(D, E, F), date(G, H, I)) :-
 	G is A + D,
 	H is B + E,
 	I is C + F.


 % -------------------------------------------------------------------
 % The following predicate relates a date to which day it is in the year
 % -------------------------------------------------------------------

 year_day(date(_, 1, Day), Day).

 year_day(date(Year, Month, Day), Year_Day) :-
 	Month > 1,
 	Prev_Month is Month - 1,
 	year_day(date(Year, Prev_Month, 1), Day_A),
 	days_in(Year, Prev_Month, Day_B),
 	Year_Day is Day_A + Day - 1 + Day_B.

 year_day(date(Year, Month, Day), Year_Day) :-
 	Month < 1,
 	Next_Month is Month + 1,
 	year_day(date(Year, Next_Month, 1), Day_A),
 	days_in(Year, Month, Day_B),
 	Year_Day is Day_A + Day - 1 - Day_B.


 % -------------------------------------------------------------------
 % The following predicate relates a year day to its month and month day
 % -------------------------------------------------------------------

 month_day(Year, Year_Day, Month, Month_Day) :-
 	Month_Lower is ((Year_Day - 1) div 31) + 1, % Lowest possible month for given day
 	Month_Upper is ((Year_Day - 1) div 28) + 1, % Highest possible month for given day
 	between(Month_Lower, Month_Upper, Month), % The right month is somewhere between
 	year_day(date(Year, Month, 1), Month_Start_Year_Day),
 	days_in(Year, Month, Month_Length),
 	Month_End_Year_Day is Month_Start_Year_Day + Month_Length - 1,
 	Month_Start_Year_Day =< Year_Day, Year_Day =< Month_End_Year_Day,
 	Month_Day is Year_Day + 1 - Month_Start_Year_Day.





/* date to day, day to date.
in the end we should probably implement this mainly with a lookup table anyway, and run the implementations just in debug mode for checking purposes. */


/*
░█▀▄░█▀█░▀█▀░█▀▀░░░▀█▀░█▀█░░░█▀▄░█▀█░█░█
░█░█░█▀█░░█░░█▀▀░░░░█░░█░█░░░█░█░█▀█░░█░
░▀▀░░▀░▀░░▀░░▀▀▀░░░░▀░░▀▀▀░░░▀▀░░▀░▀░░▀░
*/
 absolute_day(Date, Abs_Day) :-
 	ground(Date),
 	sensible_date(Date),
	date_to_absolute_day(Date, Abs_Day1),
	date_to_rata_die(Date, Abs_Day2),
	(	Abs_Day1 = Abs_Day2
	->	true
	;	throw_format('implementations of absolute_day disagree: ~q ~q ~q', [Date, Abs_Day1, Abs_Day1])),
	Abs_Day = Abs_Day1.


 % -------------------------------------------------------------------
 % Internal representation for dates is absolute day count since 1st January 0001
 % -------------------------------------------------------------------

 date_to_absolute_day(Date, Abs_Day) :-
 	((
 	Date = date(Year, Month, Day),
 	Month_A is (Year - 1) * 12 + (Month - 1),
 	Num_400Y is Month_A div (400 * 12),
 	Num_100Y is Month_A div (100 * 12),
 	Num_4Y is Month_A div (4 * 12),
 	Num_1Y is Month_A div (1 * 12),
 	Years_Day is (Num_1Y * 365) + (Num_4Y * 1) - (Num_100Y * 1) + (Num_400Y * 1),
 	Month_B is 1 + (Month_A mod 12),
 	year_day(date(Num_1Y + 1, Month_B, Day), Year_Day)
 	)->true;throw_string('absolute_day error'(Date, Abs_Day))),
 	Abs_Day is Years_Day + Year_Day.

% https://en.wikipedia.org/wiki/Rata_Die | https://en.wikipedia.org/wiki/Julian_day
 date_to_rata_die(date(Y,M,D), Abs_Day) :-
	JDN #= (1461 * (Y + 4800 + (M - 14)/12))/4 + (367 * (M - 2 - 12 * ((M - 14)/12)))/12 - (3 * ((Y + 4900 + (M - 14)/12)/100))/4 + D - 32075,
	Abs_Day #= JDN - 1721425. % 1721425 is the Julian day number for 1 January 1 CE





/*
░█▀▄░█▀█░█░█░░░▀█▀░█▀█░░░█▀▄░█▀█░▀█▀░█▀▀
░█░█░█▀█░░█░░░░░█░░█░█░░░█░█░█▀█░░█░░█▀▀
░▀▀░░▀░▀░░▀░░░░░▀░░▀▀▀░░░▀▀░░▀░▀░░▀░░▀▀▀
*/
/*
 gregorian_date(Abs_Day, Date) :-
	gregorian_date_old(Abs_Day, Date1),
	rata_die_to_gregorian_date(Abs_Day, Date2),
	(	Date1 = Date2
	->	true
	;	throw_format('implementations of rata_die disagree: ~q ~q ~q', [Abs_Day, Date1, Date2])),
	Date = Date2,
	sensible_date(Date).
*/
/*
todo tests, lookup table..
*/

 gregorian_date(Abs_Day, Date) :-
	rata_die_to_gregorian_date(Abs_Day, Date),
	sensible_date(Date).


 gregorian_date_old(Abs_Day, date(Year, Month, Day)) :-
  Z is (Abs_Day - 1),
/*	Days_1Y = 365,
	Days_4Y = 1461,
	Days_100Y = 36524,
	Days_400Y = 146097.*/
  Days_1Y is 365,
  Days_4Y is (4 * Days_1Y) + 1,
  Days_100Y is (25 * Days_4Y) - 1,
  Days_400Y is (4 * Days_100Y) + 1,

  Num_400Y is Z div Days_400Y,
  Num_100Y is (Z mod Days_400Y) div Days_100Y,
  Num_4Y is ((Z mod Days_400Y) mod Days_100Y) div Days_4Y,
  Num_1Y is (((Z mod Days_400Y) mod Days_100Y) mod Days_4Y) div Days_1Y,

  Year_Day is 1 + ((((Z mod Days_400Y) mod Days_100Y) mod Days_4Y) mod Days_1Y),
  Year is 1 + (400 * Num_400Y) + (100 * Num_100Y) + (4 * Num_4Y) + (1 * Num_1Y),
  month_day(Year, Year_Day, Month, Day).


 rata_die_to_gregorian_date(Abs_Day, date(VY, VM, VD)) :-
	JDN #= Abs_Day + 1721425,
	Vy #= 4716,
	Vv #= 3,
	Vj #= 1401,
	Vu #= 5,
	Vm #= 2,
	Vs #= 153,
	Vn #= 12,
	Vw #= 2,
	Vr #= 4,
	VB #= 274277,
	Vp #= 1461,
	VC #= -38,
	Vf #= JDN + Vj + (((4 * JDN + VB) // 146097) * 3) // 4 + VC,
	Ve #= Vr * Vf + Vv,
	Vg #= (Ve mod Vp) // Vr,
	Vh #= Vu * Vg + Vw,
	VD #= ((Vh mod Vs)) // Vu + 1,
	VM #= ((Vh // Vs + Vm) mod Vn) + 1,
	VY #= (Ve // Vp) - Vy + (Vn + Vm - VM) // Vn.


 % -------------------------------------------------------------------
 % Predicate asserts that the given absolute day resides between two Gregorian dates
 % -------------------------------------------------------------------

 day_between(Opening_Date, Closing_Date, Day) :-
 	gregorian_date(Day, Date),
 	date_between(Opening_Date, Closing_Date, Date).

 date_between(
 	/*inputs*/
 	Opening_Date,
 	Closing_Date,
 	Date
 ) :-
 	/* todo check that all uses of this pred mean to exclude the end date */
 	absolute_day(Opening_Date, Opening_Day),
 	absolute_day(Closing_Date, Closing_Day),
 	absolute_day(Date, Day),
 	Opening_Day =< Day, Day < Closing_Day.

 date_within(
 	/*inputs*/
 	Opening_Date,
 	Closing_Date,
 	Date
 ) :-
 	absolute_day(Opening_Date, Opening_Day),
 	absolute_day(Closing_Date, Closing_Day),
 	absolute_day(Date, Day),
 	Opening_Day =< Day, Day =< Closing_Day.


 % Finds the difference of 2 dates in day format
 day_diff(Date1, Date2, Days) :-
 	absolute_day(Date1, Days1),
 	absolute_day(Date2, Days2),
 	Days is Days2 - Days1.

 absolute_days_day_diff(Date1, Date2, Days) :-
 	absolute_day(Date1, Days1),
 	absolute_day(Date2, Days2),
 	Days is Days2 - Days1.

 % -------------------------------------------------------------------
 % parses date in "DD-MM-YYYY" format
 % -------------------------------------------------------------------

 parse_date_into_absolute_days(DateString, Absolute_Days) :-
 	parse_date(DateString, YMD),
 	absolute_day(YMD, Absolute_Days).

 parse_date(DateString, YMD) :-
 	(
 		parse_time(DateString, iso_8601, UnixTimestamp),
 		stamp_date_time(UnixTimestamp, DateTime, 'UTC'),
 		date_time_value(date, DateTime, YMD)
 	)
 	->
 		true
 	;
 		throw_string(['failed parsing date:', DateString]).

 format_date(Date, DateString) :-
 	format_time(string(DateString), '%Y-%m-%d', Date).

 add_days(Date, Absolute_Days, Date2) :-
 	absolute_day(Date, Day),
 	Day2 is Day + Absolute_Days,
 	gregorian_date(Day2, Date2).

  date_in_request_period(Date) :-
 	result_property(l:start_date, Start_Date),
 	result_property(l:end_date, End_Date),
 	date_within(Start_Date, End_Date, Date).

  read_date(S,P,V) :-
 	doc(S,P,O),
 	doc(O,rdf:value,V),
 	(	V = date(_,_,_)
 	->	(
 			(	V = date(1,1,1)
 			->	(
 					sheet_and_cell_string(O, Str),
 					throw_format('error reading date at ~w, found: ~q', [Str, V])
 				)
 			;	true)
 		)
 	;	(
 			sheet_and_cell_string(O, Str),
 			throw_format('error reading date at ~w, found: ~q', [Str, V])
 		)
 	).

  check_date(Date) :-
 		Date = date(_,_,_)
 	->	(
 			Date = date(1,1,1)
 			->	throw_format('error reading date: ~q', [Date])
 			;	true
 		)
 	;	throw_format('error reading date: ~q', [Date]).
