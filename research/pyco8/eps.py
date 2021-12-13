from nodes import *


class Eps(dict):


	def add_ep(s, key, guard_term):
		if key not in s:
			s.key = []
		s[key].append(guard_term)



	def is_arg_productively_different(s, old: NodeInst, now: NodeInst):

		""" if both args are existentials of the same type, (and not the exact same Thing), 'now' is considered productively different if it was asserted into existence earlier than old. That is, a recursion is allowed where the only difference between iterations is that different pre-existing existentials are considered, but a recursion is not allowed where iterations only differ because of presence of newly minted existentials. In yet other words, it's not allowed to try to prove something through invoking an existential rule ad infinitum.
		"""


		if now.is_existential:
			if old.is_existential:
				return now.existential_id < old.existential_id
			else return True

		elif now.type == 'var':
			return  old.type == 'var' and not old.is_existential

		elif now.type == 'const':
			if old.type == 'const':
				return now.value != old.value
			else return True

		else: assert False




	def is_term_productively_different(s, old, now):
		if old.len() != now.len():
			return True
		for i,old_item in enumerate(old):
			if s.is_arg_productively_different(old_item, now[i]):
				return True
		return False




	def ep_ok(s, key, guard_term):
		if key in s:
			for old in s[key]:
				if not s.is_term_productively_different(old, guard_term):
					return False
		return True


