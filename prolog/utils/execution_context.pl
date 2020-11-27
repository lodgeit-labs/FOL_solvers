/*
execution contexts: something like stack traces, but done manually:
you call:
push_context('investment calculator request')
push_context('extract accounts')
...

and then, when there's an exception, we can print a nice "processing stack":
when processing:
1) investment calculator request
2) extract accounts
3) xxx
4) yyy
exception: blablabla


plus, there's a higher level api:
c(callable): calls push_context(callable), then calls callable
c(blabla, callable): calls push_context(blabla), then calls callable
*/

/*
get_context(Ctx_list) :-
	catch(
		b_getval(context, Ctx_list),
		_,
		Ctx_list = []
	).
	*/

 get_context(Ctx_list) :-
	b_getval(context, Ctx_list).

 push_context(C) :-
	get_context(Ctx_list),
	append(Ctx_list, [C], New_ctx_list),
	b_setval(context, New_ctx_list).

 push_format(Format_string, Args) :-
 	push_context($>format(string(<$), Format_string, Args)).

 pop_context :-
	b_getval(context, Ctx_list),
	!append(New_ctx_list,[_],Ctx_list),
	b_setval(context, New_ctx_list).

 c(Context, Callable) :-
	push_context(Context),
	call(Callable),
	pop_context.

 c(Callable) :-
	c(Callable,Callable).

 cf(Callable) :-
	Callable =.. [Functor|_],
	c(Functor,Callable).



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
	->	Str = ''
	;	(
			context_string1(1, C, Item_strings),
			atomics_to_string(['during: \n' | Item_strings], Str)
		)
	).

 context_string1(Number, [C|Rest], [Str|Str_rest]) :-
	context_string2(Number, C, Str),
	Next is Number + 1,
	context_string1(Next, Rest, Str_rest).

 context_string1(_, [],[]).

 context_string2(Number, C, Str) :-
	(	atomic(C)
	->	atomics_to_string([Number, ': ', C, ' \n'], Str)
	;	format(string(Str), '~q: ~q \n', [Number, C])).

