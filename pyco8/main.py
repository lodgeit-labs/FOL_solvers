# T-SYS
# ===
#
# /* docset
#
# <Request> with <Request_graph> satisfies l:single_entity_ledger_model :-
# 	m(q(Request,l:has_sheet_instances,Instance_list,l:request_data), Request_graph),..
	
	

	
	
	

# problem:
# 	allowed only one instance of excel_sheet with type bank_statement, where sheet.data.account_name eq X
#
#
# 'all bank statement sheets have unique bank account names'(Bss) :-
# 	maplist(another_bs_with_same_name_doesnt_exist(Bss), Bss).
#
# another_bs_with_same_name_doesnt_exist(Bss, Bs) :-
# 	maplist(another_bs_with_same_name_doesnt_exist2(Bs), Bss).
#
# another_bs_with_same_name_doesnt_exist2(A, B) :-
# 	dif(A.name, B.name).
	

	
	
	
# '<X> is last cell of some list' :-
# 	X rfd:rest rdf:nil.
#
# '<X> is last cell of <List>' :-
# 	member(x, List),
# 	X rfd:rest rdf:nil.
#
# '<X> is last item of <List>' :-
# 	C is last cell of List,
# 	C rdf:first X.

	
	

	

	
# all members of a list satisfy some property:
# this is literally maplist(property, List).
# when the list is not terminated, once maplist gets to the unbound "last" item, there are two options:
# property is normal code:
# 	this will be a choicepoint. repeated recursion on multiple unknown tail items will be possible thanks to fixpoint mechanism.
# 	1) tail gets unified with nil, nothing to apply prop to, and later this branch fails
# 	2) tail gets unified with list bnode followed by nil, prop builds out an existential variable, this later fails
# 	3) tail gets unified with list bnode followed by unbound variable, this is suspended by ep, later resumed.









class Term(list):
	pass

class Rule:
	def __init__(s, head, body):
		s.head = head
		s.body = body
	def instantiate(s):
		pass

class Var(Node):



	def do_post_unification_hooks(s):
		for _ in s.do_post_unification_hooks2(s.post_unification_hooks):
			yield

	def do_post_unification_hooks2(s, hooks):
		if hooks.len() == 0:
			yield
		h = hooks[0]
		hooks = hooks[1:]
		for _ in h():
			for _ in s.do_post_unification_hooks2(hooks):
				yield


class Reasoner:

	"""
	A fixpoint reasoner for a datalog with existentials.
	"""
	
	def __init__(s, rules):
		s.rules = rules

	def query(s, q):
		while prove_term(q):
			yield q

	def prove_term(s, q):
		old_proof_tree_state = q.deep_copy()
		for proof in s.deepen_proof_tree(q):
			if q.eq(old_proof_tree_state):
				# this model stopped deepening. is it ground?
				if q.is_ground():
					yield q
			else:
				for _ in s.prove_term(q):
					yield

	def deepen_proof_tree(q):
		for _ in s.builtin(q):
			yield
		for rule_declaration in s.rules:
			rule = rule_declaration.instantiate()
			head = rule.head
			for _ in unify(q, head):
				for _ in deepen_proof_tree__body(rule.body):
					yield

	def deepen_proof_tree__body(s, body):
		if body.len == 0:
			yield
		else:
			i = body[0]
			body = body[1:]
			for _ in s.deepen_proof_tree(i):
				for _ in s.deepen_proof_tree__body(body):
					yield

	def builtin(s, q):
		functor = q[0]
		args = q[1:]
		if functor == 'dif':
			hook = lambda: dif(arg[0], arg[1])
			for arg in args:
				arg.add_post_unification_hook(hook)
			for _ in hook():
				yield
			for arg in args:
				arg.pop_post_unification_hook(hook)


	def unify(s, x, y):
		if x.type == 'var':
			x.bind(y)
			for _ in x.do_post_unification_hooks():
				yield
			x.unbind()




def dif(x,y):
	if x !== y:
		yield







