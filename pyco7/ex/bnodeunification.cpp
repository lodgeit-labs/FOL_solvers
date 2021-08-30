#include "bnodeunification.h"

BnodeUnification::BnodeUnification() {}

void Unification2::step()
{
	if (entry)
		goto* entry;
	if (x == y || *x == *y)
	{
		// reason = same things
		return &&end;
	}
	if (x->type == UNBOUND_UNIVERSAL)
	{
		x->bind(y);
		// reason = x bound to y
		return &&unbind_x;
	}
	if (y->type == UNBOUND_UNIVERSAL)
	{
		y->bind(x);
		// reason = y bound to x
		return &&unbind_y;
	}
	if (y->type == y->type)
	{
		if (x->type == CONST)
		{
			if (x->value == y->value)
			{
				// reason = "same consts";
				return &&end;
			}
			else
			{
				// reason  =("different consts: %s %s" % (val_x.value, val_y.value)),
				// xy)
				return 0;
			}
		}
		if (x->type == BNODE)
		{
			bnode_unification = new BnodeUnification(x_addr, y_addr);
			goto l_bnode_unification;
			// reason = bnodes didn not unify
			return 0;
		}
	}
	// reason = fail( ("different things: %s %s" % (val_x, val_y)), xy)
	return 0;
l_bnode_unification:;
	{
		if (bnode_unification->step())
		{
			// reason = bnodes unified
			return &&l_bnode_unification;
		}
		delete bnode_unification;
		return 0;
	}
unbind_x:;
	{
		x.unbind();
		return 0;
	}
unbind_y:;
	{
		y.unbind();
		return 0;
	}
end:
	return 0;
}
