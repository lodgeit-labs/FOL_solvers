T-SYS
===

/* docset 

<Request> with <Request_graph> satisfies l:single_entity_ledger_model :-
	m(q(Request,l:has_sheet_instances,Instance_list,l:request_data), Request_graph),..
	
	

	
	
	
*/

problem:
	allowed only one instance of excel_sheet with type bank_statement, where sheet.data.account_name eq X


'all bank statement sheets have unique bank account names'(Bss) :-
	maplist(another_bs_with_same_name_doesnt_exist(Bss), Bss).
	
another_bs_with_same_name_doesnt_exist(Bss, Bs) :-
	maplist(another_bs_with_same_name_doesnt_exist2(Bs), Bss).

another_bs_with_same_name_doesnt_exist2(A, B) :-
	dif(A.name, B.name).
	

	
	
	
'<X> is last cell of some list' :-
	X rfd:rest rdf:nil.
	
'<X> is last cell of <List>' :-
	member(x, List),
	X rfd:rest rdf:nil.
	
'<X> is last item of <List>' :-
	C is last cell of List,
	C rdf:first X.

	
	

	
	
	
	
	
all members of a list satisfy some property:
this is literally maplist(property, List).
when the list is not terminated, once maplist gets to the unbound "last" item, there are two options:
property is normal code:
	this will be a choicepoint. repeated recursion on multiple unknown tail items will be possible thanks to fixpoint mechanism.
	1) tail gets unified with nil, nothing to apply prop to, and later this branch fails
	2) tail gets unified with list bnode followed by nil, prop builds out an existential variable, this later fails
	3) tail gets unified with list bnode followed by unbound variable, this is suspended by ep, later resumed.



class Hypothesis:
	postulate: Term
	
class Fact:
	term: Term
	
class Reasoner:
	"""
	A fixpoint reasoner for a datalog with existentials.
	"""
	
	def __init__(s, kb):
		s.kb = kb
		
	def query(s, q):
		for _ in s.deepen_proof_tree(q)
			proof_after_one_cycle = q.deep_copy()
				for _ in s.deepen_proof_tree(q)
					if q.eq(
			
	
















	
	
	
	
	
	
