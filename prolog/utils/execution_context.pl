/*

execution contexts: something like stack traces, but higher level and done manually:
you call:
push_context('investment calculator request')
push_context('extract accounts')
...

and then, when there's an exception, we can print a nice "processing stack":
when processing:
1) investment calculator request
2) extract accounts
3) ...
4) ...
exception: blablabla

*/


 get_context(Ctx_list) :-
	(	nb_current(context, Ctx_list)
	->	true
	;	Ctx_list = []).

%	catch(
%		b_getval(context, Ctx_list),
%		_,
%		Ctx_list = []
%	).

 get_context_depth(D) :-
	b_current_num_with_default(context_depth, 0, D).

 get_context_trace(X) :-
	(	nb_current(context_trace, X)
	->	true
	;	X = []).
%	catch(
%		b_getval(context_trace, X),
%		_,
%		X = []
%	).


flag_default('ENABLE_CONTEXT_TRACE_TRAIL',false).

:- if(env_bool('ENABLE_CONTEXT_TRACE_TRAIL',true)).

 context_trace_init_trail_0 :-
	Fn = 'context_trace_trail.txt',
	Fnn = loc(file_name, Fn),
	(	absolute_tmp_path(Fnn, loc(absolute_path, Trail_File_Path))
	->	true
	;	Trail_File_Path = Fn),
	open(Trail_File_Path, write, Trail_Stream, [buffer(line)]),
	b_setval(context_trace_trail, Trail_Stream).

 context_trace_trail(Term) :-
	nb_getval(context_trace_trail, Stream),
	(	Stream \= []
	->	(
			statistics(process_epoch, E),
			get_time(TimeStamp),
			Ts is TimeStamp - E,
			format(Stream, 'T~3fs ~w~n', [Ts,Term]),
			%writeq(Stream, Term),
			%writeln(Stream, '\n'),
			flush_output(Stream)
		)
	;	true).

 context_trace_trail__push_context(C) :-
	(
		(
			%context_string(Str),
			get_context_trace(Trace0), term_string(Trace0, Str),
			context_trace_trail(Str)
		)
		;
		(
			context_trace_trail(unwind(C)),
			fail
		)
	).

 context_trace_trail__pop_context :-
 	context_trace_trail(pop).



:- else.

 context_trace_trail__push_context(_).
 context_trace_trail__pop_context.
 context_trace_init_trail_0.
 context_trace_trail(_).

:- endif.

flag_default('ENABLE_CONTEXT_TRACE',true).

:- if(env_bool('ENABLE_CONTEXT_TRACE', false)).

 push_context(_).
 push_format(_,_).
 pop_context.
 pop_format.

:- else.

 push_context(C) :-
	get_context(Ctx_list),
	get_context_depth(Depth),
	get_context_trace(Trace),
	append(Ctx_list, [C], New_ctx_list),
	New_depth is Depth + 1,
	append([(Depth,C)], Trace, New_trace),
	nb_setval(context_trace, New_trace),
	nb_setval(context_depth, New_depth),
	nb_setval(context, New_ctx_list),
	context_trace_trail__push_context(C).


 push_format(Format_string, Args) :-
 	maplist(round_term, Args, Args2),
 	push_context($>format(string(<$), Format_string, Args2)).

 pop_format :-
	pop_context.

 pop_context :-
	nb_getval(context, Ctx_list),
	get_context_depth(Depth),
	New_depth is Depth - 1,
	nb_setval(context_depth, New_depth),
	!append(New_ctx_list,[_],Ctx_list),
	nb_setval(context, New_ctx_list),
	context_trace_trail__pop_context.

:- endif.

 ct(Context) :-
	push_context(Context),
	pop_context.

%:- meta_predicate 'c'(?, 0).
 ct(Context, Callable) :-
	push_context(Context),
	call(Callable),
	pop_context.

%:- meta_predicate 'c'(?, 1, ?).
 ct(Context, Callable, Arg1) :-
	push_context(Context),
	call(Callable, Arg1),
	pop_context.

%:- meta_predicate 'c'(?, 2, ?, ?).
 ct(Context, Callable, Arg1, Arg2) :-
	push_context(Context),
	call(Callable, Arg1, Arg2),
	pop_context.

%:- meta_predicate 'c'(?, 3, ?, ?, ?).
 ct(Context, Callable, Arg1, Arg2, Arg3) :-
	push_context(Context),
	call(Callable, Arg1, Arg2, Arg3),
	pop_context.

%:- meta_predicate 'c'(?, 4, ?, ?, ?, ?).
 ct(Context, Callable, Arg1, Arg2, Arg3, Arg4) :-
	push_context(Context),
	call(Callable, Arg1, Arg2, Arg3, Arg4),
	pop_context.

%:- meta_predicate 'c'(0).
 c(Callable) :-
	push_context(Callable),
	call(Callable),
	pop_context.

 c(Callable, Arg1) :-
	push_context(Callable),
	call(Callable, Arg1),
	pop_context.

%:- meta_predicate 'c'(?, 2, ?, ?).
 c(Callable, Arg1, Arg2) :-
	push_context(Callable),
	call(Callable, Arg1, Arg2),
	pop_context.

%:- meta_predicate 'c'(?, 3, ?, ?, ?).
 c(Callable, Arg1, Arg2, Arg3) :-
	push_context(Callable),
	call(Callable, Arg1, Arg2, Arg3),
	pop_context.

%:- meta_predicate 'c'(?, 4, ?, ?, ?, ?).
 c(Callable, Arg1, Arg2, Arg3, Arg4) :-
	push_context(Callable),
	call(Callable, Arg1, Arg2, Arg3, Arg4),
	pop_context.



%:- meta_predicate 'cf'(3).
 cf(Callable) :-
	Callable =.. [Functor|_],
	ct(Functor,Callable).

%:- meta_predicate 'c'(1, ?).
 cf(Callable, Arg1) :-
	Callable =.. [Functor|_],
	ct(Functor,Callable, Arg1).

%:- meta_predicate 'c'(2, ?, ?).
 cf(Callable, Arg1, Arg2) :-
	Callable =.. [Functor|_],
	ct(Functor,Callable, Arg1, Arg2).

%:- meta_predicate 'c'(2, ?, ?, ?).
 cf(Callable, Arg1, Arg2, Arg3) :-
	Callable =.. [Functor|_],
	ct(Functor,Callable, Arg1, Arg2, Arg3).

%:- meta_predicate 'c'(2, ?, ?, ?, ?).
 cf(Callable, Arg1, Arg2, Arg3, Arg4) :-
	Callable =.. [Functor|_],
	ct(Functor,Callable, Arg1, Arg2, Arg3, Arg4).



% dummy (or debug?) context wrapper

 cd(_Context, Callable) :-
	call(Callable).


/*
┏━┓╺┳╸┏━┓╻┏┓╻┏━╸╻┏━╸╻ ╻╻┏┓╻┏━╸
┗━┓ ┃ ┣┳┛┃┃┗┫┃╺┓┃┣╸ ┗┳┛┃┃┗┫┃╺┓
┗━┛ ╹ ╹┗╸╹╹ ╹┗━┛╹╹   ╹ ╹╹ ╹┗━┛
for what it's worth. Should be superseded by a nice svelte Rdf viewer UI
*/

 context_string(Str) :-
	get_context(C),
	context_string(C, Str).

 context_string(C,Str) :-
	(	C = []
	->	Str = ""
	;	(
			context_string1(1, C, Item_strings),
			atomics_to_string(Item_strings, Str)
		)
	).

 context_string1(Number, [C|Rest], [Str|Str_rest]) :-

 	nonvar(Rest),
 	Rest \= [],
 	
 	format(string(NumberStr), '~q:', [Number]),
	context_string2(NumberStr, C, Str),
	Next is Number + 1,
	context_string1(Next, Rest, Str_rest).

 context_string1(_, [C], [Str]) :-
	context_string2('->', C, Str).

 context_string1(_, [],[]).

 context_string2(Number, C, Str) :-
	(	atomic(C)
	->	atomics_to_string([Number, ' ', C, ' \n'], Str)
	;	format(string(Str), ' ~q \n', [C])).

