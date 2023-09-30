#include "thing.h"
#include "coro.h"

Thing::Thing()
{
}

Thing *Thing::getValue()
{
	Thing *v = this;
	while (v->type == EXISTENTIAL && v->binding != nullptr)
		v = v->binding;
	return v;
}

/*
def get_value(x):
    if type(x) is Atom:
        return x
    v = x.bound_to
    if v:
        return get_value(v)
    else:
        return x



def unify(_x, _y):
    assert(isinstance(_x, Arg))
    assert(isinstance(_y, Arg))
    x = get_value(_x.thing)
    y = get_value(_y.thing)
    return unify2(_x, _y, x, y)
*/
