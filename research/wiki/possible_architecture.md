(this is old)

add another python_server django "app" called "excel_request_gateway"
	parse the incoming xml
		wheres our parse-by-schema code?
	detect the request type
	if its one of: loan, etc
		just pass-through to prolog (for now)
	if its livestock calculator:
		invoke livestock calculator app/code
			invokes formula solver

	if its ledger:
		stick the request into the db
		invoke livestock transaction type inferencer service, with the uri of the request
			does some inferencing, 
			produces a slightly modified request,
			sticks it into the db, returns the uri
			future performance considerations:
				if this pipeline should become high-throughput, we'll have to make some adjustments:
					add option to run this in batches, as just "add_more_transactions" or something
					add option to update the input request data destructively (and return the same uri?)

		invoke the main ledger processing service part 1:
			probably lets keep this prolog code monolithic for now:
				preprocess_s_transactions
				process_livestock
				checks and stuff
			saves GL entries list + account hierarchy into the db, returns uri
		invoke reporting (report generation) service, or services?






