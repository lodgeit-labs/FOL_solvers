/* todo: https://github.com/LodgeiT/labs-accounts-assessor/wiki/SWIPL-and-prolog-notes#atoms-vs-strings */


 replace_nonalphanum_chars_with_underscore(Atom1, Atom2) :-
	atom_chars(Atom1, Atom1_Chars),
	maplist(replace_nonalphanum_char_with_underscore, Atom1_Chars, Atom2_Chars),
	atom_chars(Atom2, Atom2_Chars).

 capitalize_atom(Atom1, Atom2) :-
	atom_chars(Atom1, Atom1_Chars),
	[First_Char|Atom1_Chars_Rest] = Atom1_Chars,
	char_type(Upper, to_upper(First_Char)),
	[Upper|Atom1_Chars_Rest] = Atom2_Chars,
	atom_chars(Atom2, Atom2_Chars).

 replace_nonalphanum_char_with_underscore(Char1, Char2) :-
	char_type(Char1, alnum)
		->
	Char1 = Char2
		;
	Char2 = '_'.


:- meta_predicate replace_chars_in_atom(1, +, +, -).

 replace_chars_in_atom(Predicate, Replacement, Atom_In, Atom_Out) :-
	atom_chars(Atom_In, Atom1_Chars),
	maplist(replace_char_if_not(Predicate, Replacement), Atom1_Chars, Atom2_Chars),
	atom_chars(Atom_Out, Atom2_Chars).

 replace_char_if_not(Predicate, Replacement, Char_In, Char_Out) :-
	call(Predicate, Char_In) -> Char_Out = Char_In ; Char_Out = Replacement.


:- meta_predicate filter_out_chars_from_atom(1, +, -).

 filter_out_chars_from_atom(Predicate, Atom_In, Atom_Out) :-
	atom_chars(Atom_In, Atom1_Chars),
	findall(
		[Char],
		member(Char, Atom1_Chars),
		Char_Lists),
	maplist(atom_chars, Atom1_Char_Atoms, Char_Lists),
	exclude(Predicate, Atom1_Char_Atoms, Atom2_Char_Atoms),
	atomic_list_concat(Atom2_Char_Atoms, Atom_Out).



 is_url(URI) :-
	% todo atom_prefix is deprecated
	atom_prefix(URI,"http").



 icase_endswith(String, End) :-
	string_lower(String, String2),
	sub_string(String2, _,_,0,End).


 call_with_string_read_stream(String, Callable) :-
	setup_call_cleanup(
		new_memory_file(X),
		(
			open_memory_file(X, write, W),
			write(W, String),
			close(W),
			open_memory_file(X, read, R),
			call(Callable, R),
			close(R)),
		free_memory_file(X)).

 stt(St, Str) :-
	new_memory_file(F),
	open_memory_file(F, write, W),
	print_prolog_backtrace(W, St),
	close(W),
	open_memory_file(F, read, R),
	read_stream_to_codes(R,C),
	string_codes(Str,C),
	close(R),
	free_memory_file(F).


 'use grammar to interpret text'(Grammar, Text) :-
	phrase(Grammar, $>string_codes(Text)).

 'use grammar to generate text'(Grammar, Text) :-
	phrase(Grammar, Codes),
	string_codes(Text, Codes).

 fs(Format, Args, Result) :-
 	format(string(Result), Format, Args).
