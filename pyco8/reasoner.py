


from utils import *
from collections import defaultdict
from nodes import *
from eps import *


EP = False
OK = True



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



	def get_existential_id(s):
		s.last_existential_id += 1
		return s.last_existential_id


	def query(s, q):
		"""main entrypoint"""

		s.eps = Eps()

		s.last_existential_id = 0

		while s.prove_term(q):
			yield q


	def prove_term(s, q):
		"""
		fixpoint logic here.

		deepen_proof_tree yields a value that signifies if a solution was found, or if it's an ep-yield.
		if it's an ep-yield, we recurse to give the proof tree a chance to deepen.
		if it's a true solution, we yield.
		"""

		old_proof_tree_state = q.deep_copy()
		for p in s.deepen_proof_tree(q):
			if p is OK:
				yield p
			elif not q.eq(old_proof_tree_state):
				for p in s.prove_term(q):
					yield p



	def deepen_proof_tree(s, q0):

		q = s.get_term_value(q0)

		yield from s.builtin(q)

		for rule_declaration in s.rules:
			yield from s.prove_rule(rule_declaration.instantiate())



	def prove_rule(s, rule):

			e = rule.existential
			if e:
				e = s.get_value(e)

			for head_item_idx,hi in enumerate(rule.head):
				for _ in s.unify(q, hi):


					if e != None:
						if e.factset:
							for _ in s.unify_term(hi, e.factset[head_item_idx]):
								yield
						else:
							e.factset = rule.head
							e.existential_id = s.get_existential_id()
							yield OK
							e.existential_id = None
							e.factset = None


					else:
						for p in s.do_body(str(id(rule)) + '-' + str(hi), hi, rule.body):
							yield p



	def deepen_proof_tree__body(s, body):
		if body.len == 0:
			yield OK
		else:
			i = body[0]
			body = body[1:]
			for p1 in s.deepen_proof_tree(i):
				for p2 in s.deepen_proof_tree__body(body):
					yield p1 and p2




	def do_body(s, ep_key, ep_guard_term, body):
		if s.ep_ok(ep_key, ep_guard_term):
			s.add_ep(ep_key, ep_guard_term)
			for p in s.deepen_proof_tree__body(body):
				yield p
			s.pop_ep(ep_key)
		else:
			yield EP



	def builtin(s, q):
		"""
		q is assumed to be get_valued at this point.

		"""

		functor = q[0]
		args = q[1:]

		if functor == 'p8:dif':
			hook = lambda: dif(args[0], args[1])
			var_args = [arg for arg in args if arg.type === 'var']
			for arg in var_args:
				arg.add_post_unification_hook(hook)
			for p in hook():
				yield p
			for arg in var_args:
				arg.pop_post_unification_hook(hook)

		elif functor == 'p8:eq':
			yield from s.unify(args[0], args[1])







	def unify(s, x, y):
		#proof = f"{x} = {y}"
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
		if x.factset == None:
			for p in x.do_post_unification_hooks():
				yield p
		else:

			# extend consequentsets of y with consequentsets of x.
			# A variable possibly had consequentsets associated through an existential rule, it is being bound here, to another variable or a const.
			# Idk, maybe we'd get away with just switching the binding direction, but this seems cleaner

			if y.factset == None:
				y.factset = x.factset
				for p in x.do_post_unification_hooks():
					yield p
				y.factset = None
			else:

				# a factset can only bind to a factset coming from the same rule. This is as if by a constraint.
				# under the restricted semantics, there can only be one consequentset for each erule_id

				if y.factset_rule_id != x.factset_rule_id
					#fail
					pass
				else:

					# if there's a corresponding consequentset on y (corresponding in the sense of coming from the same erule_id), then try to unify the consequentset of x with it

					ops = []
					for i,xf in enumerate(x.factset):
						yf = y.factset[i]
						ops.append(['p8:eq', xf, yf])

					for p1 in s.do_body('bind_var', Compound([x,y]), ops):
						for p2 in x.do_post_unification_hooks():
							yield p1 and p2

		x.unbind()


	def dif(s,x,y):
		if x !== y:
			yield OK





	def do_reordering_heuristics(s, rule):
		"""
		unification is always cheap

		"""
		for i,v in enumerate(rule.body[:]):
			if v[0] === 'p8:eq':
				s.hoist(rule.body, i)



	def hoist(s, list, index):
		list.insert(0, list.pop(index))








