 how to let prolog fill in  arbitrary unknowns, or detect errors, as you've been talking about, andrew. One step towards that is estabilishing a concept of a "document". The livestock sheet is one "document" in the prolog code. We will try to work in a framework where there aren't separate definitions for input and output formats. Any cell not filled in by user can be inferred, given enough other data and right equations. But the endpoint has to see that the cell was left empty. 
as on the equation solving itself, andrew, prolog clpq/r integrates cleanly with all other prolog code. A theoretical limit is that the equations mostly have to be linear, in terms of complexity. Mostly everything we support so far is, except maybe some computation in depreciation. But this won't break the general system, i think it just means that we won't be able to infer in arbitrary directions over that depreciation equation, so we'll have to add some more special-purpose code
2:58 AM
and the final piece is proofs/explanations. Clpq/r is an exact opposite of a system that would allow this, but if my idea is right, we'll keep track of all equations we "submit" into it, and when a result is found, we'll submit them into a piece of code (probably in python) that will do this, if the complexity of the system of equations allows. This is more strict than that they just have to be linear, this is that they have to form a DAG. And the dag is also essentially the explanation
again everything we have mostly seems to be in that category
Andrew, 3:00 AM
Ok
3:00 AM
ie, Livestock_COGS = Opening_and_purchases_value - Stock_on_hand_at_end_of_year_value - Killed_for_rations_value, Killed_for_rations_value = Killed_for_rations_count * Average_cost, etc, it is a tree, no loops, no sideway-joins
Andrew, 3:00 AM
Is it going to be combersome to deploy?
3:23 AM
depends on the extent i think. For a simple system like the livestock standalone calculator here, it seems straightforward. Extending on that, i'm considering a ledger-livestock request where the bank statement is fully specified. Here, all the principles should still work just as well, given some code cleanup/refactoring. Beyond that, let's say we have some complete livestock sheets and we have a "hole" in a bank statement, where we want to infer missing transactions. At this level of complexity, prolog would, un-directed, just fall into the first trap it finds. Let's say we dont know how many transactions are missing in the bank statement. That would be exactly the first trap, prolog would just fall into an infinite cycle trying to enumerate all numbers, so, we would need some extra code to direct the search. Kind of coincidentally, bob has been putting some work into just that, on a personal project, but i'd rather concern myself with the simpler problems first

If there is a range of missing transactions then we should have an alert i.e. every day should start with the balance of the previous day. If it does not you can 'fill' the hole with a quasi transaction & consider this as a 'suspense' fact. The HP Calculator works with a sequence of facts. A fact may include 60 payments. So one or more payments coincide with the timeframe when the suspense fact is recognised. So unless the HP has been terminated, then the suspense fact can be assumed to include the HP payments. The suspense fact value is then modified by the discovered amount.






inputLivestockDataStandalone:
	name
	year
	average cost
	purchases
	...
		
	
r(inputLivestockDataStandalone, outputLivestockDataStandalone)



bank statement -> livestock_trading_aggr

r(input_livestock_data_ledger, livestock_trading_aggr, gl livestock adjustment entries)


bs(st, subsidiary_ledgers_in, subsidiary_ledgers_out, exchange_rates_in,



s_transactions optionally with missing unit counts <-> s_transactions

hp input <-> ideal bank statement entries

hp input, actual bank statement -> correction entries



inputs_gl():-
	running this backwards would expose the biggest search space, + subsidiary ledgers infor would be mostly missing
	suppose we have: 
		exchange rates
		gl with only basic tx types
		possibly some complete bs's
		one bs missing
	gl_to_inputs_helper:
		for all txs with same date:
			can we rely on ordering or have to try all permutations?
	technicalities:
		use an open list and memberchk's for Gl_Entries


