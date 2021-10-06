

class NodeInst:
	type = None

	def __init__(s):
		s.factset = None
		s.existential_id = None

	@property
	def is_existential(s):
		return s.type == 'var' and s.factset is not None


class ConstInst(NodeInst):
	type = 'const'

	def __init__(s, value):
		super().__init__()
		s.value = value



class VarInst(NodeInst):
	type = 'var'

	def __init__(s):
		super().__inst__()
		s.value = None
		s.post_unification_hooks = []

	def add_post_unification_hook(s,hook):
		s.post_unification_hooks.append(hook)

	def pop_post_unification_hook(s,hook):
		assert(hook == s.post_unification_hooks.pop())

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



class VarIdx(int):
	"""

	used in locals template, contains the offset to a given variable (in locals instance)

	"""
	pass
