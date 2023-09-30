from tri_struct import *


def tri_struct_from_json(j):
	if type(j) is list:
		return [tri_struct_from_json(x) for x in j]
	if type(j) is dict:
		return tri_struct_from_json([k,tri_struct_from_json(v) for k,v in j.items()])
	else:
		return j
