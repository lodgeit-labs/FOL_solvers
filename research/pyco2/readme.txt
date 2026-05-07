# introduction
this is a sucessor to https://github.com/koo5/AutoNomic-pyco
While pyco only achieved a limited form of order invariance with 'EP_YIELD's only in rules producing existentials, this version 'ep_yield's anywhere, and repeats the whole query for as long as the proof tree is growing. It is perhaps an iterative deepening search.

Another difference in semantics from pyco is currently lack of "second_chance" logic (allowing extra depth of search when recursing on lists), and missing "prune_duplicate_results" option.

This version is implemented in (swi)prolog, so we can experiment with integrating CLP, and we can call native clauses.

Integration with the "doc" system is tbd, as are many other things.


# running it

```
reset;echo -e "\e[3J";   swipl -O -s pyco2_test2.pl -g "test(q3(_,_)),halt" 2>&1 | tee x  
cat x | grep "result:"
```

# random notes


## possibly useful for ep check:

https://www.swi-prolog.org/pldoc/man?section=compare


## optimization:

http://irnok.net:3030/help/source/doc/home/prolog/ontology-server/ClioPatria/lib/semweb/rdf_optimise.pl

https://books.google.cz/books?id=oc7cBwAAQBAJ&pg=PA26&lpg=PA26&dq=prolog++variable+address&source=bl&ots=cDxavU-UaU&sig=ACfU3U0y1RnTKfJI58kykhqltp8fBNkXhA&hl=en&sa=X&ved=2ahUKEwiJ6_OWyuPnAhUx-yoKHZScAU4Q6AEwEHoECAkQAQ#v=onepage&q=prolog%20%20variable%20address&f=false

=====

?sts0 prepreprocess ?sts1
?sts1 preprocess ?txs



{?sts0 prepreprocess ?sts1} <=
{
    ?sts0 first ?st0
    ?st0 action_verb "livestock_sell"

    .....

    ?sts0 rest ?stsr
    ?stsr prepreprocess ?sts1r.

two approaches to optimization:
    follow the data:
        ?txs are bound, so call ?sts1 preprocess ?txs first
    ep-yield earlier:
        as soon as we're called with only vars?


### pre-evaluation, inlining:


take a call to fr(L,F,R), the definition of fr/3 is:
`
	fr(L,F,R)
		,first(L, F)
		,rest(L, R)
`
we can pre-compile the definition into something like:
`
	fr(_{first:F,rest:R},F,R)
`
, and further inline that into the calling place.
This would need to be revised if we change bnode representation/semantics into something like:
`
(
	Bn = _{first:F,rest:R}
;
	(
		first(Bn, F)
		,rest(Bn, R)
	)
)
`
there is a kindof generalized way that pre-evaluation can be done, which i think we touched on in univar,
let's say you have the declaration fr(L,F,R), and also have two pre-asserted rdf lists in your kb:
`
first(list1, x).
rest(list1, nil).

first(list2, x).
rest(list2, list2_2).
first(list2_2, y).
rest(list2_2, nil).
`
you don't know if your program will ever call fr(Unbound_var1, Unbound_var2, Unbound_var3), but if you have the cpu time up-front, you can still pre-evaluate the predicate as if it was called with all vars unbound. You replace the origial declaration with:
`
fr(_{first:F,rest:R},F,R).
fr(list1, x, nil).
fr(list2, x, list2_2).
fr(list2_2, y, nil).
`
not sure if simply running the query and collecting the results would work for more complex cases, ie recursion, ep-yields, or if some interpretation is needed.





## syntax, notation

you can have multiple heads in a prolog clause. This would be one way to write pyco rules "natively"

[a,b] :- writeq(xxx).

?- clause([X|XX],Y).
X = a,
XX = [b],
Y = writeq(xxx).





## debugging, visualizations:
univar pyco outputs, for example, kbdbgtests_clean_lists_pyco_unify_bnodes_0.n3:
	describes rule bodies and heads in detail.
	Terms just simple non-recursive functor + args, but thats a fine start.
	structure of locals..(memory layout), because pyco traces each bind, expressed by memory adressess. We could probably just not output that and the visualizer would simply not show any binds but still show the proof tree.
	eventually, a script is ran: converter = subprocess.Popen(["./kbdbg2jsonld/frame_n3.py", pyin.kbdbg_file_name, pyin.rules_jsonld_file_name])
	converts the n3 to jsonld, for consumption in the browser app.
	we cant write json-ld from swipl either, so, i'd reuse the script.

	store traces in doc? nah, too much work wrt backtracking
	but we'll store the rules/static info described above, either in doc or directly in rdf db,
	then save as something that the jsonld script can load, and spawn it.

	trace0.js format:
		S() is a call to a function in the browser. This ensures that the js file stays valid syntax even on crash.




# cacheing / "memoization"
..

# ...
... :- 
	global(Job),
	sheets(Job, Sheets),


---	

model(Model) :-
	q(Model, a, model).


sheet(Name, Sheet) :-
	model(Model),
	q(Model, sheets, Sheets),
	member(Sheet, Sheets),
	q(Sheet, name, Name).
	

model_start_date(Start_Date) :-
	sheet(report_details, D),
	q(D, ic:from, Start_Date).
---
========



query:



=>

q(Model sheets Sheets), 
q(Sheets first report_details1), 
q(Sheets rest Sheets1), 
q(Sheets1 first bank_statement1), 
q(Sheets1 rest Sheets2), 
q(Sheets2 first ?BS), 
q(Sheets2 rest nil), 
is_valid_ic2_model(Model).

the first part might be expressed in compact rdf:
?Model sheets (report_details1 bank_statement1 [a balance_sheet]), 

the second part does not have a direct representation in rdf:
is_valid_ic2_model(Model).

(could use the obvious Model is_valid_model true, ...

====

existentials:
 
 - Model:

	 - sheets
	   - note: i think i'll aim to represent all inputs and outputs as excel sheet rdf, for a baseline. All input sheet types are already defined as such, and reports can be represented as such. 

	 - states
	   - note: the model represents a series of states representing different steps of automatic accounting. Each state is a product of a particular domain specific phase, for exampe phase of SMSF profit redistribution.

	 - accounts
	    - note: account hierarchy as extracted from XML or XBRL files. It's not clear how our sub-accounts (currently "generated" at run time according to traded units) will fit into this.

	 - units
	    - note: traded unit classifications and values
	    

 - State:
     - note: holds the "s_transactions" and "gl_transactions" posted in a particular accounting phase.

	 - "s_transactions"
	   - note: the high-level "bank statement" transactions. Better name needed. Business events is too broad.
	   - type: list
	   - origin: reorder_sts..

	 - gl_transactions
	   - note: the general ledger transactions
	   - type: list
	   - origin: this is currently called "preprocess"

   
 
   
===





is_valid_ic2_model(Model) :-
	   
   







smsf_final_states(Model) :-
	


	q(Model, states, States),
	member(State, States),
	q(State, phase, smsf_profit_redistribution).






-====
fixed layouter python:

phases_center = 100,100
phases_radius = 50
phases_angle = 2 * math.pi / len(phases)

for i,phase in enumerate(phases):
	phase.position = (phases_center[0] + phases_radius * math.cos(phases_angle * i), phases_center[1] + phases_radius * math.sin(phases_angle * i))
	
	
	
	
	
	


======


# avoiding negation by using nonequality constraints:
(it seems potentially beneicial to avoid negation simply to keep the proof tree simpler)


smsf :-
		maplist({ != smsf_members_sheet }, Sheets)
		maplist({ != smsf_distribution_sheet }, Sheets)
		maplist({ != smsf_taxes_sheet }, Sheets)
	;
		(
			member(Sheet, Sheets),
			q(Sheet, name, smsf_members_sheet),
			q(Sheet, name, smsf_distribution_sheet),
			q(Sheet, name, smsf_taxes_sheet),
		).


======


next, should we simplify by avoiding an open-ended sheet list - it's not clear how it would become closed in the proof tree, perhaps by unifying it with a concatenation of lists of specific sheet types, ie: (pseudocode)

```
Smsf_sheets = [smsf_members_sheet, smsf_distribution_sheet, smsf_taxes_sheet],
Bank_statement_sheets = [bank_statement1, bank_statement2, bank_statement3],
Sheets = Smsf_sheets + Bank_statement_sheets + [report_details, unit_values].
```




====









====

existentials:
 
 - Model:


     - reasoner
	   - note: info about the reasoner that produced the model
  
	 - sheetset
	 

	 - states
	   - note: the model represents a series of states representing different steps of automatic accounting. Each state is a product of a particular domain specific phase, for exampe phase of SMSF profit redistribution.

	 - accounts
	    - note: account hierarchy as extracted from XML or XBRL files. It's not clear how our sub-accounts (currently "generated" at run time according to traded units) will fit into this.

	 - units
	    - note: traded unit classifications and values
	    

 - State:
     - note: holds the "s_transactions" and "gl_transactions" posted in a particular accounting phase.

	 - "s_transactions"
	   - note: the high-level "bank statement" transactions. Better name needed. Business events is too broad.
	   - type: list
	   - origin: reorder_sts..

	 - gl_transactions
	   - note: the general ledger transactions
	   - type: list
	   - origin: this is currently called "preprocess"

   
 
   
===


vvv it might be an interesting idea to propagate the truthness of a head statement into the body and deeper recursively. These would not fare as logical conditions proper, it could be a good syntax to obtain head variables down the proof tree?


====	


% we will not force rules into rdf, they will be like datalog:

q:

% generate all valid models
model(M),
% whose bank_statement_sheets list unifies with our input sheets
bank_statement_excels(M, Bank_statement_excels),

% where we have two bank statements
fr(Bank_statement_excels, Bs0, Bank_statement_excels1),
fr(Bank_statement_excels1, Bs1, rdf:nil),
% where both are exactly the user input sheets
eq(Bs0, bank_statement_0),
eq(Bs1, bank_statement_1),
%

% and we have report_details excel
report_details_sheet(M, Report_details_excel),
eq(Report_details_excel, report_details_1),
%

% and that's it. We'll check the resulting balance sheet visually in the rdf explorer.






====


	model(M),
	states(M, States),

	bank_statement_sheets(M, Bank_statement_excel),
	report_details_sheet(M, Report_details_excel),

	balance_sheet(M, Balance_sheet_excel)


:-
%	is_list(Bank_statement_sheets),
	
	nth(0, States, State0),
	nth(1, States, State1),
	
	state_stransactions_from_bank_statements(Bank_statement_sheets, State0),
	post_book_closing(State1, Report_details_sheet, State2),
	balance_sheet(State2, Balance_sheet_excel).	
	
	

	
stransactions_from_bank_statements(Bank_statement_sheets, State) :-
	state_stransactions(State, Sorted_stransactions),
	maplist(stransactions_from_bank_statement, Bank_statement_sheets, Stransactionses),
	concat(Stransactionses, Stransactions),
	sort_stransactions(Stransactions, Sorted_stransactions),





append(ListOfLists, List) :-
    must_be(list, ListOfLists),
    append_(ListOfLists, List).

append_([], []).
append_([L|Ls], As) :-
    append(L, Ws, As),
    append_(Ls, Ws).




% finishme
must_be(_,_).





% These semantics do not cover what one would expect of sort/2. After a call to sort, the Out list should indeed be sorted if "ground" and if nonground, there should be constraints between the members regarding their future ordering that make any unification about to violate that ordering fail. (can that be implemented? I think so, we have attributed variables!) or else sort/2 should throw a fat exception when it can't take out this insurance on the future.
% - https://www.swi-prolog.org/pldoc/man?predicate=sort/2















list_minimum([L|Ls], Min) :- foldl(minimum_, Ls, L, Min).

minimum_(A, B, Min) :- Min #= min(A, B).







% naivesort?

sorted(A, B) :-
	ordered(B),
	permutation(A, B).
	

ordered([]).
ordered([X|Xs]) :- 
	ordered(Xs),
	maplist(>=(X), Xs).


permute([], []).
permute([X|Rest], L) :-
    permute(Rest, L1),
    select(X, L, L1).




===

oscillation - the system must spot when it's going back and forth between two body items. It's a little more complex than that.

Then, it's a question how systematically we want to traverse the space of body item order permutations, and whether we want to parallelize or rollback and work in the same thread.

===

visualization - 

what goes into the kb: rule descriptions











