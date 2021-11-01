from sympy import *

print(solve(
	[
		Eq(
			17,
			Symbol('blabla2')
		),
		Eq(
			7*Symbol('blabla2') + (Symbol("assets") /4- Symbol("liabilities")*Symbol('blabla1')),
			Symbol("equity")*Symbol('blabla2')
		)
	],
	Symbol("blabla1")
))
# how is blabla2 not replaced by 17????
