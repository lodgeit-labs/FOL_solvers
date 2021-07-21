#include "thing.h"
#include "coro.h"


Thing::Thing()
{

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



def unifier2(arg_x, arg_y, val_x, val_y):
    """
    doc:
    in addition to the usual stuff:
    a bnode binds to another bnode, if they both come from the same original rule, and from the same variable name
    variables bind to bnodes just like to constants, constants and variables dont unify
    """
    xy = (arg_x, arg_y)
    yx = (arg_y, arg_x)
    if val_x is val_y:
        return success("same things", xy)
    x_is_var = type(val_x) is Var
    y_is_var = type(val_y) is Var
    if x_is_var and not val_x.bnode():
        return val_x.bind_to(val_y, xy)
    elif y_is_var and not val_y.bnode():
        return val_y.bind_to(val_x, yx)
    elif y_is_var and x_is_var and val_x.is_a_bnode_from_original_rule == val_y.is_a_bnode_from_original_rule and val_x.is_from_name == val_y.is_from_name:
        return val_y.bind_to(val_x, yx)
    elif type(val_x) is Atom and type(val_y) is Atom:
        if val_x.value == val_y.value:
            return success("same consts", xy)
        else:
            return fail(nolog or ("different consts: %s %s" % (val_x.value, val_y.value)), xy)
    else:
        return fail(nolog or ("different things: %s %s" % (val_x, val_y)), xy)
*/


/*
task<> unify2(Thing x_addr, Thing y_addr) {
    Thing x = *x_addr;
    Thing y = *y_addr;

    if (x == y)
        co_yield;
    else
    {
        if (x.type == UNBOUND_UNIVERSAL)
        {
            x.bind(y);
            co_yield;
            x.unbind();
        }
        else
        {
            if (y.type == UNBOUND_UNIVERSAL)
            {
                y.bind(x);
                co_yield;
                y.unbind();
            }


        }

    }

}

*/
