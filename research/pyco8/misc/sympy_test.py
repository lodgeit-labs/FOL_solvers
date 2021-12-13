from sympy import *

print(solve(
	[
		Eq(
			17,
			Symbol('blabla1')
		),
		Eq(
			7*Symbol('blabla2') + (Symbol("assets") /4- Symbol("liabilities")*Symbol('blabla1')),
			Symbol("equity")*Symbol('blabla2')
		)
	],
	Symbol("blabla1")
))
 # how is this an empty set???
 
