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






class NodeInst:
	pass

class ConstInst(Node):
	type = 'const'
	def __init__(s, value):
		s.value = value.

class VarInst(Node):
	type = 'var'
	def __init__(s):
		s.value = None

	def add_post_unification_hook(s,hook):
		s.post_unification_hooks.append(hook)

	def pop_post_unification_hook(s,hook):
		assert(hook === s.post_unification_hooks.pop())

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





class Term(list):
	"""
	compound term. first item is functor, the rest are args
	"""
	pass










class RuleDecl:

	def __init__(s, jsn):

		s.locals_dict = {}
		s.locals_count = 0

		s.head = s.extend_locals_template_with_term(jsn.head)
		s.body = [s.extend_locals_template_with_term(bi) for bi in jsn.body]


	def extend_locals_template_with_term(s, t):
		if t.type == 'compound':
			return Term([s.extend_locals_template_with_term(arg) for arg in t.items])
		elif t.type == 'var':
			name = t.name
			if name not in s.locals_dict:
				s.locals_dict[name] = s.locals_count
				s.locals_count += 1
			return VarIdx(s.locals_dict[name])
		elif t.type == 'const':
			return ConstInst(t.value)


	def instantiate(s):
		r = RuleInst()
		r.decl = s
		r.locals = [VarInst() for _ in range(s.locals_count)]



class RuleInst:
	pass






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
		if functor == 'p8:dif':
			hook = lambda: dif(arg[0], arg[1])
			var_args = [arg for arg in args if arg.type === 'var']
			for arg in var_args:
				arg.add_post_unification_hook(hook)
			for _ in hook():
				yield
			for arg in var_args:
				arg.pop_post_unification_hook(hook)


	def unify(s, x, y):
		if x.type == 'var':
			for _ in s.bind_var(x,y):
				yield
		elif y.type == 'var':
			for _ in s.bind_var(y,x):
				yield


	def bind_var(s, x, y):
		x.bind(y)
		for _ in x.do_post_unification_hooks():
			yield
		x.unbind()


	def dif(s,x,y):
		if x !== y:
			yield







def do_reordering_heuristics(rule):
	"""
	unification is always cheap

	"""
	for i,v in enumerate(rule.body[:]):
		if v[0] === 'p8:eq':
			hoist(rule.body, i)



def hoist(list, index):
	list.insert(0, list.pop(index))
