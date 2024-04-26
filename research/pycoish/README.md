
# styles of equations in robust:
From the point of view of a possible reimplementation in pycoish, or in pure prolog with tabling.
Also notes on current traceability of the calculations.

## account "rollups"
This is somewhat traceable thanks to vecs and report entries being rdf.
```prolog
 balance_sheet_entry2(Static_Data, Account_Id, Entry) :-
	% find all direct children sheet entries
	% fixme:  If Key is unbound, all associations in Dict are returned on backtracking. The order in which the associations are returned is undefined. (in doc.pl). This leads to random order of entries in the report.
	account_direct_children(Account_Id, Child_Accounts),
	maplist(!balance_sheet_entry2(Static_Data), Child_Accounts, Child_Entries),
	% find child balances
	maplist(!report_entry_total_vec,Child_Entries,Child_Balances),
	maplist(!report_entry_transaction_count,Child_Entries,Child_Counts),
	% balance for this account including subaccounts (sum all transactions from beginning of time)

	account_own_transactions_sum(Static_Data.exchange_rates, Static_Data.end_date, Static_Data.report_currency, Account_Id, Static_Data.end_date, Static_Data.transactions_by_account, Own_Sum, Own_Transactions_Count),
	
	vec_sum([Own_Sum | Child_Balances], Balance),
	%format(user_error, 'balance_sheet_entry2: ~q :~n~q~n', [Account_Id, Balance]),
	sum_list(Child_Counts, Children_Transaction_Count),
	Transactions_Count is Children_Transaction_Count + Own_Transactions_Count,

```


```prolog

account_own_transactions_sum(Exchange_Rates, Exchange_Date, Report_Currency, Account, Date, Transactions_By_Account, Sum, Transactions_Count) :-
	add_days(Date,1,Date2),
	transactions_by_account(Transactions_By_Account, Account, Account_Transactions),
	findall(
		Transaction,
		(
			member(Transaction, Account_Transactions),
			transaction_before(Transaction, Date2)
		),
		Filtered_Transactions
	),
	length(Filtered_Transactions, Transactions_Count),
	transaction_vectors_total(Filtered_Transactions, Totals),
	vec_change_bases(Exchange_Rates, Exchange_Date, Report_Currency, Totals, Sum)
	%,format(user_error, 'account_own_transactions_sum: ~q :~n~q ~n ~q ~n', [Account, Sum, Filtered_Transactions])
	.
```

```prolog
 transactions_before(Account_Transactions,Date2,Filtered_Transactions) :-
	findall(
		Transaction,
		(
			member(Transaction, Account_Transactions),
			transaction_before(Transaction, Date2)
		),
		Filtered_Transactions
	).
```
...all in all, it's mostly just recursion on lists of transactions (dynamic length) and on account tree (could possibly be compiled beforehand).

## crosschecks:
```prolog
		equality(
			account_balance(reports/pl/current, rl('Distribution_Revenue')),
			fact_value(aspects([concept - ($>rdf_global_id(smsf_distribution_ui:distribution_income))]))),
```

```prolog
 	doc_new_uri("crosscheck", Crosscheck_uri),
 	
	evaluate(Crosscheck_uri, Sd, A, A2),
	evaluate(Crosscheck_uri, Sd, B, B2),
	doc_add(Crosscheck_uri, kb:left_side, A2),
	doc_add(Crosscheck_uri, kb:right_side, B2),

	(
	 crosscheck_compare(A2, B2)
	->
	 (
	  Equality_Str = '=',
	  Status = 'ok',
	  Diff = []
	 )
	;
	 (
	  Equality_Str = '≠',
	  Status = 'error',
      vec_sub(A2, B2, Diff)
	 )
	),


	Check = check{op: Equality_Str, a:A, b:B},
	Evaluation = evaluation{op: Equality_Str, a:A2, b:B2},

	C = crosscheck{check:Check, evaluation:Evaluation, status:Status, diff: Diff},

	round_term(Check, Check_round), % avoid variables in the term.

	doc_add(Crosscheck_uri, kb:crosscheck_check, Check_round),
	doc_add(Crosscheck_uri, kb:crosscheck_evaluation, Evaluation),
	doc_add(Crosscheck_uri, kb:crosscheck_status, Status),
	doc_add(Crosscheck_uri, kb:crosscheck_diff, Diff).
	
```

interpreting the expression language:
```prolog
 evaluate(Crosscheck_uri, Sd, Term, Value) :-
	(	evaluate2(Crosscheck_uri, Sd, Term, Value)
	->	true
	;	Value = evaluation_failed(Term, $>gensym(evaluation_failure))).


 evaluate2(_Crosscheck_uri, Sd, report_value(Key), Values_List) :-
	path_get_dict(Key, Sd, Values_List).
		

 evaluate2(Crosscheck_uri, Sd, account_balance(Report_Id, Acct), Values_List) :-
	
	/* get report out of static data, such as "reports/pl/current" */
	/* it's a dict, such as creted by balance_sheet_at, where entries is a list of make_report_entry uris */
	*path_get_dict(Report_Id, Sd, Report_wrapper),

	assertion(is_dict(Report_wrapper)),
	Entries = Report_wrapper.entries,
	assertion(is_list(Entries)),

	resolve_account(Acct, Account_uri),
	accounts_report_entry_by_account_uri(Entries, Account_uri, Entry),
	entry_normal_side_values(Entry, Account_uri, Values_List0),
	assertion(t(Values_List0, l:vec)),
	
	doc_add(Entry, kb:crosscheck, Crosscheck_uri), % this would be extraneous if vectors were actualy rdf objects everywhere. 
	
	vec_sum([Values_List0], Values_List).  % isnt this extraneous?



  evaluate2(_Crosscheck_uri, _, fact_value(Aspects), Values_List) :-
	evaluate_fact2(Aspects, Values_List).



 evaluate2(_Crosscheck_uri, _, Vec, Vec) :-
	t(Vec, l:vec).


```

crosschecks are stored rdf, but only as complex objects.


## investment report:
(currently operates with raw vectors).

some automation based on tables..
```prolog
 totals(Rows, Totals) :-
	/*these are computed automatically*/
 	table_totals(Rows,
	[
		gains/rea/market_foreign, gains/rea/market_converted, gains/rea/forex,
		gains/unr/market_foreign, gains/unr/market_converted, gains/unr/forex,
		opening/total_cost_foreign, opening/total_cost_converted,
		closing/total_cost_foreign, closing/total_cost_converted,
		on_hand_at_cost/total_converted_at_purchase,
		on_hand_at_cost/total_converted_at_balance,
		on_hand_at_cost/total_forex_gain
	], Totals0),
	/*these are not computed automatically*/
	Totals = Totals0.put(
		gains/realized_total, Realized_Total).put(
		gains/unrealized_total, Unrealized_Total).put(
		gains/total, Total).put(
		unit, 'Total'),
	vec_add_(Totals0.gains.rea.market_converted, Totals0.gains.rea.forex, Realized_Total),
	vec_add_(Totals0.gains.unr.market_converted, Totals0.gains.unr.forex, Unrealized_Total),
	vec_add_(Realized_Total, Unrealized_Total, Total).
```

"direct" clpr calculations over value terms... 
```prolog
	optional_currency_conversion(Exchange_Rates, Opening_Date, Investment_Currency, Report_Currency, Opening_Currency_Conversion),
	optional_currency_conversion(Exchange_Rates, End_Date, Investment_Currency, Report_Currency, Closing_Currency_Conversion),

	Closing_Unit_Price_Foreign_Amount = End_Unit_Price_Amount,

	Closing_Unit_Price_Foreign = value(Investment_Currency, Closing_Unit_Price_Foreign_Amount),
	{Investment_Currency_Current_Market_Value_Amount = Count * Closing_Unit_Price_Foreign_Amount},
	Investment_Currency_Current_Market_Value = value(Investment_Currency, Investment_Currency_Current_Market_Value_Amount),
	[Report_Currency_Unit] = Report_Currency,

	{Current_Market_Value_Amount = Count * Closing_Unit_Price_Converted_Amount},
	Current_Market_Value = value(Report_Currency_Unit, Current_Market_Value_Amount),

	optional_converted_value(Closing_Unit_Price_Foreign, Closing_Currency_Conversion, Closing_Unit_Price_Converted),

	value_multiply(Opening_Unit_Cost_Foreign, Count, Opening_Total_Cost_Foreign),
	value_multiply(Opening_Unit_Cost_Converted, Count, Opening_Total_Cost_Converted),


```

## SMSF

(we parse some data from smsf input sheets in an extra way, supporting string to number conversion inside prolog, for some reason.)

We reassert relevant input values as fact objects with concept=the prop.

```prolog
	assert_doc_value_as_fact(Item, Prop, Default_currency,
		aspects([
			concept - ($>rdf_global_id(Concept)),
			unit - Unit
```

shorthand expression syntax for simple facts where concept is the only discerning attribute.
```prolog
	!computed_unit_fact(Unit,
		(smsf_distribution_ui:accrual)
		=
		(smsf_distribution_ui:net_cash_distribution)
		-
		(smsf_distribution_ui:bank)),
```

generate a list where each item is essentially a matcher(a query) for a single fact.

"""produce all aspectses to later look up in GL and assert"""
```prolog

	!maplist(!smsf_member_details_report_aspectses3(Member),
	[
		x(final/bs/current, 'Opening_Balance', []),
		/* effect etc should be something else than an aspect. A tag perhaps. */
		x(final/bs/current, 'Transfers_In', [effect - addition]),
		x(final/bs/current, 'Pensions_Paid', [effect - subtraction]),
		x(final/bs/current, 'Benefits_Paid', [effect - subtraction]),

```

```prolog
	'='(
		Facts,
		[
			aspects($>append([
				report - Report,
				account_role - ($>atomic_list_concat([Concept, '_-_Preserved/Taxable'])) / Member,
				concept - smsf/member/gl/Concept,
				phase - 'Preserved',
				taxability - 'Taxable',
				member - Member
			], Additional_aspects)),

```

interpreting the expression language:
```prolog
exp_compute(A = B) :-
	assertion(A = aspects(_)),
	!exp_eval(B, B2),
	!facts_by_aspects(A, Already_asserted),
	(	Already_asserted = []
	->	true
	;	throw_string('exp_compute error')),
	!make_fact(B2, A).

exp_eval(X, X) :-
	t(X, l:vec).	

exp_eval(X, X2) :-
	X = aspects(_),
	!evaluate_fact2(X,X2).

exp_eval(A + B, C) :-
	exp_eval(A, A2),
	exp_eval(B, B2),
	vec_add(A2, B2, C).

```

a language for more complex, vector, expressions (smsf_incompe_tax_v2.pl) 

```prolog

 solve_rule('='(X, Y)) :-
	X = aspects(X_aspects),
	v2_exp_eval(Y, Y_val, Evaluation),
	make_fact(Y_val, X_aspects, Uri),
	doc_add(Uri, l:formula, Y),
	doc_add(Uri, l:evaluation, Evaluation).

 v2_exp_eval(X, X, Str) :-
	t(X, l:vec),
	val(X, V),
 	term_string(V, Str).

 v2_exp_eval(X, X2, Uri) :-
	X = aspects(_),
	v2_evaluate_fact2(X, X2, Uri).

 v2_exp_eval(sum(Summants), Result, Uri) :-
	!maplist(v2_exp_eval,Summants,Results,Uris),
	!vec_sum(Results, Result),
	doc_new_(l:eval, Uri),
	doc_add(Uri, l:constituents, Results),
	maplist(doc_add(Uri, l:constituent), Uris),
	doc_add(Uri, l:result, Result).

 v2_exp_eval(B, B, Str) :-
	((rational(B),!);(number(B),!)),
	term_string(B,Str).

 v2_exp_eval(Op, C, Uri) :-
 	Op =.. [Binop, A, B],
	v2_exp_eval(A, A2, A2_uri),
	v2_exp_eval(B, B2, B2_uri),
	v2_binop(Binop, A2, B2, C),
	doc_new_(l:eval, Uri),
	doc_add(Uri, l:op1_value, A2),
	doc_add(Uri, l:op2_value, B2),
	doc_add(Uri, l:op1, A2_uri),
	doc_add(Uri, l:op2, B2_uri),
	doc_add(Uri, l:result, C).

 v2_binop('+', A2, B2, C) :-
	vec_add(A2, B2, C).


```

There is essentially a reimplementation of crosschecking, which can be explained by the fact that SMSF is implemented with a more advanced representation of values than crosschecks are:
```prolog
 check_entered_unit_fact_matches_computed(Default_currency, Unit, Item, Prop, Entered) :-
	(	read_value_from_doc_string(Item, Prop, Default_currency, Entered_value)
	->	(
			!make_fact(Entered_value, aspects([
				concept - ($>rdf_global_id(Entered)),
				unit - Unit
			])),
			!entered_computed_soft_crosscheck(
				aspects([
					unit - Unit,
					concept - ($>rdf_global_id(Entered))])
				=
				aspects([
					unit - Unit,
					concept - ($>rdf_global_id(Prop))]))
		)
	;	true).



```

report rendering:
```prolog
		column{
			concept:($>rdf_global_id(smsf_distribution_ui:accrual)),
			title:"Resolved_Accrual",
			options:options{implicit_report_currency:true}},

```


```prolog
	Rows0 = [
		[text('Total Current Year Capital Gains'),
			aspects([concept - ($>rdf_global_id(smsf_distribution_ui:total_capital_gains_losses))])]],
	Rows1 = [
		[text('Less: Current Year Capital Losses'),
			aspects([concept - ($>rdf_global_id(smsf_distribution_ui:capital_losses))])]],
	!rows_total(Rows0, Rows0_vec),
	!rows_total(Rows1, Rows1_vec),
	vec_sub(Rows0_vec,Rows1_vec,Vec0),
	!make_fact(Vec0,
		aspects([concept - ($>rdf_global_id(smsf_computation:taxable_net_capital_gains))])),
	Rows2 = [
		[text('Taxable Net Capital Gains'),
			aspects([concept - ($>rdf_global_id(smsf_computation:taxable_net_capital_gains))])
	]],


```

```prolog

 smsf_income_tax_report_v2(Tbl_dict) :-
	% todo unify concept names with uris/namespaces

	Rows0 = [
		[text('Benefits Accrued as a Result of Operations before Income Tax'),
			aspects([
				report - before_smsf_income_tax/pl/current,
				account_role - 'Comprehensive_Income'])]
	],

	Rules0 = [
		aspects([concept - (smsf/income_tax/'Taxable Trust Distributions (Inc Foreign Income & Credits)')])
		=
		sum(
			[
				aspects([concept - ($>gu(smsf_distribution_ui:non_primary_production_income))]),
				aspects([concept - ($>gu(smsf_distribution_ui:franked_divis_distri_including_credits))]),
				aspects([concept - ($>gu(smsf_distribution_ui:assessable_foreign_source_income))])
			]
		)
	],


```

note that the calculated values eventually feed back into the next system "state" through new GL txs

```prolog
	!maplist(!smsf_distribution_tx(Default_currency, End_Date, Item),
		[dist{
			prop: smsf_distribution_ui:accrual,
			a:'Distribution_Revenue'/Unit/'Resolved_Accrual',
			dir:crdr,
			b:'Distribution_Receivable'/Unit,
			desc:"Distributions Accrual entry as per Annual tax statements"},

```







