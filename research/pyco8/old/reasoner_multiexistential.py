


from utils import *



class NodeInst:
	pass



class ConstInst(NodeInst):
	type = 'const'
	def __init__(s, value):
		s.value = value



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
		"""recurse on remaining hooks. proof is a conjunction of all hook's truth"""
		if hooks.len() == 0:
			yield
		h = hooks[0]
		hooks = hooks[1:]
		for _ in h():
			for _ in s.do_post_unification_hooks2(hooks):
				yield



class Term(list):
	"""
	compound term. first item of list is functor, the rest are args
	"""
	pass



class RuleDecl:

	def __init__(s, jsn):

		jsn = tri_struct_from_json(jsn)

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
		s.rules = [RuleDecl(r) for r in rules]


	def query(s, q):
		s.existentials = defaultdict(lambda: defaultdict(list))

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
			for hi in rule.head:
				for _ in unify(q, hi):
					for _ in s.ep_guarded(rule, hi):
						for _ in s.deepen_proof_tree__body(rule.body):
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
			erule_id = args[3]
			rule_head_all_terms = args[4:]

			existential_id = id(the_existential) # assumed to be unique, immutable, and, for constants, to have a 1:1 mapping to the constant's value

			consequentsets = s.existentials[existential_id][erule_id]
			if not consequentsets.len():
				existentials[existential_id] = {
					erule_id: [{
						'rule_head_all_terms': rule_head_all_terms,
					}]
				}
				yield
				consequentsets.pop()

			else:
				for consequentset in s.existentials[existential_id][erule_id]:
					#each consequentset is a list of terms that comprise the previously fired existential rule's head
					for _ in s.unify_term(rule_head_term, consequentset['rule_head_all_terms'][rule_head_term_idx]):
						yield






	def unify(s, proof, x, y):
		proof = f"{x} = {y}"
		if id(x) == id(y):
			yield
		elif x.type == 'var':
			for _ in s.bind_var(x,y):
				yield
		elif y.type == 'var':
			for _ in s.bind_var(y,x):
				yield


	def bind_var(s, x, y):
		x.bind(y)
		for _ in x.do_post_unification_hooks():

			# extend consequentsets of y with consequentsets of x.
			# A variable possibly had consequentsets associated through an existential rule, it is being bound here, to another variable or a const.
			# Idk, maybe we'd get away with just switching the binding direction, but this seems cleaner

			y_existentials = s.existentials[id(y)]
			old = y_existentials

			# x_existentials is a dict from erule_id
			x_existentials = s.existentials[id(x)]

			# under the restricted semantics, there can only be one consequentset for each erule_id
			for erule_id,v in x_existentials.items():
				# if there's a corresponding consequentset on y (corresponding in the sense of coming from the same erule_id), then try to unify the consequentset of x with it
				if y_existentials[erule_id].len() != 0:
					ops.append(['=', x_existentials[erule_id][0], y_existentials[erule_id][0])
				else:
					y_existentials[erule_id].extend(v)

			r = RuleDecl({
				'head': [],
				'body':
					[
						{
							'type':'compound',
							'items':i
						} for i in ops
					]
			}).instantiate()

			for _ in s.deepen_proof_tree__body(r.body):
				yield

			s.existentials[id(y)] = old


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


