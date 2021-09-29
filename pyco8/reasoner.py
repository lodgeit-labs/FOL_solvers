


from utils import *



class NodeInst:
	pass


class ConstInst(NodeInst):
	type = 'const'
	def __init__(s, value):
		s.value = value.


class VarInst(NodeInst):
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


class RuleInst:
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







class Reasoner:

	"""
	A fixpoint reasoner for a datalog with existentials.
	"""

	def __init__(s, rules):
		s.rules = [RuleDecl(r) for r in tri_struct_from_json(rules)]


	def query(s, q):
		s.existentials = defaultdict(lambda: defaultdict({}))

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

		if functor == 'p8:is_not_literal':

			# similar to above


		if functor == 'p8:exist':

			the_existential = args[0]
			rule_head_term_idx = args[1]
			rule_head_term = args[2]
			existential_rule_id = args[3]
			rule_head_all_terms = args[4:]

			existential_id = id(the_existential) # assumed to be unique, immutable, and, for constants, to have a 1:1 mapping to the constant's value
			consequentsets = s.existentials[existential_id][existential_rule_id]
			if existential_id not in existentials:
				existentials[existential_id] = {
					existential_rule_id: {
						'rule_head_all_terms': rule_head_all_terms,
					}
				}
				yield
				existentials.remove(existential_id)
			elif existential_rule_id not in existentials[existential_id]:
				existentials[existential_id][existential_rule_id] = {
					'rule_head_all_terms': rule_head_all_terms,
				}
				yield
				existentials[existential_id].remove(existential_rule_id)
			else:
				for consequentset in existentials[existential_id][existential_rule_id]:
					#each consequentset is a list of terms that comprise the previously fired existential rule's head
					for _ in s.unify_term(rule_head_term, consequentset['rule_head_all_terms'][rule_head_term_idx]):
						yield






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

			if id(x) in s.existentials:
				s.existentials[id(y)][

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





















		# if functor == 'p8:exist':
		# 	arg = q[1]
		# 	rule_head_term_idx = q[2]
		# 	rule_head_term = q[3]
		# 	rule_head_all_terms = q[4:]
		# 	if arg.type === 'var':
		#
		# 		# register a new existential
		#
		# 		existential = Existential()
		# 		existentials = s.existentials_existentials
		# 		existentials[existential.id] = existential
		# 		existential.facts = facts
		#
		# 		for _ in s.bind_var(arg, exisential):
		# 			yield
		#
		#
		# 	elif arg.type ==='existential':
		# 		for _ in s.unify_term(rule_head_term, existential.facts[rule_head_term_idx]):
		# 			yield
		#
		#
		# 	elif arg.type ==='const':
		#
		# 		existentials = s.existentials_consts
		#


