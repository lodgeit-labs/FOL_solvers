


from utils import *
#from collections import defaultdict
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
			yield from s.prove_rule(q, rule_declaration.instantiate())



	def prove_rule(s, q, rule):

			e = rule.existential
			if e:
				e = s.get_value(e)

			for head_item_idx,hi in enumerate(rule.head):
				for _ in s.unify(q, hi):

"""
	L first F, L rest R :-
		dif(L, nil).
---	
	L first F, L rest R, L a list :-
		dif(L, nil).
	nil a list.
---

account tree facts for year Y and rphase P(Facts) :-
	maplist(account tree fact(Y, P, Facts), Account_tree_concepts),
	maplist(smsf         fact(Y, P, Facts), Account_tree_concepts),
	

account tree fact(Y, P, Facts, Concept) :-
	F has_concept Concept,
	F has_rphase P,
	F has_year Y,
	member(F, Facts)

smsf fact(Y, P, Facts, Concept) :-
	Concept has_smsf_member_dimension false.
smsf fact(Y, P, Facts, Concept) :-
	Concept has_smsf_member_dimension true,
	smsf_members(Members),
	maplist('member has smsf equity facts'(Y, P, Facts), Members, Values),
	math(sum(Values), Total),
	
	member(Total_fact, Facts),
	F has_concept Concept,
	F has_rphase P,
	F has_year Y,
	\+F has_smsf_member _,
	F has_value Votal.
	
'member has smsf equity facts'(Y,P,Facts, Member,V) :-
	member(F, Facts),
	F has_concept Concept,
	F has_rphase P,
	F has_year Y,
	F has_smsf_member Member,
	F has_value V.
^^^^^^^
V should be a rdf:value, for the usual purpose of avoiding statements about literals
--
math(sum(Values) = Total)
the equation expression can be a light syntax sugar for invocations of a rule that allows formulas to exist.  





"""


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
			hook = lambda: s.dif(args[0], args[1])
			var_args = [arg for arg in args if arg.type == 'var']
			for arg in var_args:
				arg.add_post_unification_hook(hook)
			for p in hook():
				yield p
			for arg in var_args:
				arg.pop_post_unification_hook(hook)

		elif functor == 'p8:eq':
			yield from s.unify(args[0], args[1])

		elif functor == 'p8:math':
			yield from p8math(args)



	def p8math(args):
		"""
		we will use built-in "constraints" - invisible
		X = (Y + Z)

		inputs:
			formula -->
				term compop term
			compop -->
				=
				>
				<
				>=
				<=
			term -->
				fact_selector
				const
				term op term
			op -->
				+
				-
				*
				/


		[
			a formula;
			compop p8math:lt;
			term1 [
				a p8facts:selector;
				p8facts:selector_name "xxxxx";
		...
		]

		maplist(formula_application, Formulas, Applications)...

		Facts p8:eq (
			[
				concept xxx;
				value yyy;
				year_dimension zzz;
			]
			...
		)...

		formula_application(Formula, Application) :-

		formula_has_selectors(...

		formula_with_bound_selectors(Facts, Formula_template, Formula_instance) :-
			traverse formula tree, copying it into _instance, and when we run into a selector:
			member(Fact, Facts),
			selector_matches_fact(Selector, Fact)




		==================





		facts, templates, [application|applications] :-
			member(template, templates),
			formula_with_bound_selectors(facts, template, application).


====


	facts <Facts> without values are <Fw>


====







ledger model <Model> :-
	<Model> has facts <Facts>
	ledger formulas are <Formulas>
	<Model>, given facts <Facts>, and formulas <Formulas>, has applications <Apps>.


<Model>, given facts <Facts>, and formulas <Formulas>, has applications <Apps> :-
	is_set(Apps),
	application list cell <Apps> with previous item <_>, given <Facts>, <Formulas>.


application list cell <Cell> with previous item <Prev>, given <Facts>, <Formulas> :-
	/*
	Prev keeps the recursion from ep'ing, and only allows it to continue with each item unique.

	The problem with this rule is that in backtracking, we only want the hopefully first answer, which *hopefully* would be the one with the longest list of applications. In other words, there is no nonextra-logical way to constrain the list to be the "longest possible".
	*/

	Cell first App,
	Cell rest Rest,
	application(Formulas, Facts, App),
	(App > Prev ; App = Prev)
	application list cell <Rest> with previous item <App>, given <Facts>, <Formulas>.



application(<Formulas>, <Facts>, <App>) :-
	member(Formula, Formulas),
	application2(Formula, Facts, App).


/* here we match on Formula's tree to produce an application */

application2(<Formula>, <Facts>, <App>) :-
	Formula	a equality
	Formula args Args
	maplist(expression_applied(Facts), Args, Applied_args)
	App a equality
	App args Applied_args.


expression_applied(<Facts>, <Exp>, <Applied>) :-
	/*
	find a fact by properties
	*/

	Exp a selector
	member(Fact,Facts)
	Exp properties Sp
	Fact properties Fp
	/*if they're ordered, i guess*/
	Sp = Fp
	Applied = Fact



<Application> > <Application2> :-
	some simple ordering of Application trees, like,
	equality before lt and gt,
	math functions before leaf nodes,
	selectors before constants,
	etc.


====

nevermind, let's do induction on formulas:
	each formula:
		applies
		or
		doesn't apply

	applies if:
		each selector is bound (ignoring actual fact values for now)

	doesn't apply if:
		each selector is paired with a hypothetical fact and the fact is dif'd to all real facts

^^ nvm



==============







termination:
the introduction of a helper argument for overthrowing ep-checking can possibly be seen equivalent to proving termination of a function? Should we look at ep-cuts as bugs?



====




the list of facts is terminated by the same predicate that concludes that no more applications can be made. Or at least, the open list where formulas can generate new facts into.


for each formula:
	for each selector:
		partition the list of facts into those that match and those that dont (those that dif)

===========





=============
===

	The requirement that each item should be unique could probably be formalized like this:

	set(List, Cell) :-
		/* probably with the help of a marker symmetric to nil:*/

		Prev rdf:rest Cell
		maplist_backwards(cell_has_item_different_from(This_item), Prev)
		...

	cell_has_item_different_from(Cell, Diff) :-
		Cell rdf:first X
		dif(X, Diff).
	...
===





xbrl (formulas) semantics are their own inference model:
	iterate over all facts
		iterate over all formulas
			match input facts
			produce new facts
	repeat until no new facts are being added


but we want to model everything with rules interpreted in one semantic, that of the datalog with E-rules, so that the application of formulas can be just a one part of a larger search. For example, it is unknown how many persons there are in a SMSF fund, so the engine goes back and forth between


i can't seem to be able to figure out how to model the application cycle within the datalog's semantics.


why i'm trying to stick to the semantics:
1) it has the power to infer back and forth between rules with mathematical calculations and rules about structure of the result (model).
2) there is a clear set of possible outcomes:
* all results found
* no results found
* still running

"no results found" is very interesting for us, as it translates to "your inputs are invalid, they fail to validate against your (mathematical and structural) rules", and it specifically doesn't translate to: we're not sure if this engine can handle the task, maybe your inputs are valid but the engine can't make the connection.




3 options:
### run the application engine extralogically
it would be able to run only if some conditions are satisfied, namely, ...


###








members of a fund:

the assets of the fund must equal the sum of equities of its members

may be expressed as a summation calculation in xbrl


the interesting case that we should be able to solve is when we dont know how many members there are


Ledger model valid :-
	Ledger facts Facts
	Ledger smsf_members Members
	smsf_rules Facts Members


smsf_rules Facts Members :-
	member(Member, Members),
	member(Fact, Facts),
	Fact attributes [member Member, concept "equity"]









whatever :-
	member(X, List),
	member(Y, List),





smsf_stuff :-
	value($>get_optional_singleton_sheet_data(smsf_ui:members_sheet), Members),
	maplist(smsf report must have member facts(Facts), Members).

smsf report must have member facts(Facts, Member) :-
	!maplist(!smsf_member_details_report_aspectses3(Facts, Member),
	[
		x(final/bs/current, 'Opening_Balance', []),
		/* effect etc should be something else than an aspect. A tag perhaps. */
		x(final/bs/current, 'Transfers_In', [effect - addition]),
		x(final/bs/current, 'Pensions_Paid', [effect - subtraction]),
		x(final/bs/current, 'Benefits_Paid', [effect - subtraction]),
		x(final/bs/current, 'Transfers_Out', [effect - subtraction]),
		x(final/bs/current, 'Life_Insurance_Premiums', [effect - subtraction]),
		x(final/bs/current, 'Share_of_Profit/(Loss)', [effect - addition]),
		x(final/bs/current, 'Income_Tax', [effect - subtraction]),
		x(final/bs/current, 'Contribution_Tax', [effect - subtraction]),
		x(final/bs/current, 'Internal_Transfers_In', [effect - addition]),
		x(final/bs/current, 'Internal_Transfers_Out', [effect - subtraction])
	],
	Aspectses0),
	%smsf_member_details_report_aspectses6(Member, Aspectses1),....
	!append($>flatten(Aspectses0), $>flatten(Aspectses1), Aspectses).

smsf_member_details_report_aspectses3(Member, x(Report, Concept, Additional_aspects), Facts) :-
	/*
	these accounts are all subcategorized into phase and taxability in the same way, so we generate the aspect sets automatically
	*/
	'='(
		Facts,
		[
			aspects($>append([
				report - Report,
				concept - smsf/member/gl/Concept,
				phase - 'Preserved',
				taxability - 'Taxable',
				member - Member
			], Additional_aspects)),
			aspects($>append([
				report - Report,
				account_role - ($>atomic_list_concat([Concept, '_-_Preserved/Tax-Free'])) / Member,
				concept - smsf/member/gl/Concept,
				phase - 'Preserved',
				taxability - 'Tax-Free',
				member - Member
			], Additional_aspects)),
			aspects($>append([
				report - Report,
				account_role - ($>atomic_list_concat([Concept, '_-_Unrestricted_Non_Preserved/Taxable'])) / Member,
				concept - smsf/member/gl/Concept,
				phase - 'Unrestricted_Non_Preserved',
				taxability - 'Taxable',
				member - Member
			], Additional_aspects)),
			aspects($>append([
				report - Report,
				account_role - ($>atomic_list_concat([Concept, '_-_Unrestricted_Non_Preserved/Tax-Free'])) / Member,
				concept - smsf/member/gl/Concept,
				phase - 'Unrestricted_Non_Preserved',
				taxability - 'Tax-Free',
				member - Member
			], Additional_aspects)),
			aspects($>append([
				report - Report,
				account_role - ($>atomic_list_concat([Concept, '_-_Restricted_Non_Preserved/Taxable'])) / Member,
				concept - smsf/member/gl/Concept,
				phase - 'Restricted_Non_Preserved',
				taxability - 'Taxable',
				member - Member
			], Additional_aspects)),
			aspects($>append([
				report - Report,
				account_role - ($>atomic_list_concat([Concept, '_-_Restricted_Non_Preserved/Tax-Free'])) / Member,
				concept - smsf/member/gl/Concept,
				phase - 'Restricted_Non_Preserved',
				taxability - 'Tax-Free',
				member - Member
			], Additional_aspects))
		]
	).

	!doc_new_uri(fact, Uri),
	!doc_add(Uri, rdf:type, l:fact),
	!doc_add(Uri, l:vec, $>flatten([Vec])),
	!doc_add(Uri, l:aspects, Aspects),





member(I, L) :-
	L first I.

member(I, L) :-
	L rest R,
	member(I, R).






member(I, L) :-
	L first I.

member(I, L) :-
	L rest R,
	member(I, R).











		"""



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

				if y.factset_rule_id != x.factset_rule_id:
					pass #fail
				else:

					# if there's a corresponding consequentset on y (corresponding in the sense of coming from the same erule_id), then try to unify the consequentset of x with it

					ops = []
					for i,xf in enumerate(x.factset):
						yf = y.factset[i]
						ops.append(['p8:eq', xf, yf])

					for p1 in s.do_body('bind_var', Term([x,y]), ops):
						for p2 in x.do_post_unification_hooks():
							yield p1 and p2

		x.unbind()


	def dif(s,x,y):
		if x != y:
			yield OK



	def get_value(s, v):
		if v.type == 'var':
			return s.get_value(v.value)
		return v



	def do_reordering_heuristics(s, rule):
		"""
		unification is always cheap

		"""
		for i,v in enumerate(rule.body[:]):
			if v[0] == 'p8:eq':
				s.hoist(rule.body, i)



	def hoist(s, list, index):
		list.insert(0, list.pop(index))









