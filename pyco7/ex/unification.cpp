#include "unification.h"

Unification::Unification() {}

void Unification2::step()
{
	switch (entry)
	{
	case 0:
	{
		Thing x = *x_addr;
		Thing y = *y_addr;

		if (x == y)
		{
			//reason = same things
			return 1000000;
		}
		if (x.type == UNBOUND_UNIVERSAL)
		{
			x.bind(y);
			//reason = x bound to y
			return 10;
		}
		if (y.type == UNBOUND_UNIVERSAL)
		{
			y.bind(x);
			//reason = y bound to x
			return unbind_y;
		}
		if (y.type == y.type)
		{
			if (x.type == CONST)
			{
				if (x.value == y.value)
				{
					//reason = "same consts";
					return 100;
				}
				else
				{
					//reason  =("different consts: %s %s" % (val_x.value, val_y.value)), xy)
					return 0;
				}
			}
			if (x.type == BNODE)
			{
				bnode_unification = new BnodeUnificationo(x_addr, y_addr);
				if (bnode_unification->step())
				{
					//reason = bnodes unified
					return 5;
				}
				delete bnode_unification;
				// reason = bnodes didn not unify
				return 0;
			}
		}
		//reason = fail( ("different things: %s %s" % (val_x, val_y)), xy)
		return 0;
	}
	case 5:
	{
		if (bnode_unification->step())
			return 5;
		delete bnode_unification;
		return 0;
	}
	case 10:
	{
		x.unbind();
		return 0;
	}
	case 20:
	{
		y.unbind();
		return 0;
	}
	case 100:
		return 0;
	}
	return 0;
}
