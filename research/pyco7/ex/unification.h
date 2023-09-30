#ifndef UNIFICATION_H
#define UNIFICATION_H

#include "bnodeunification.h"
#include "coro.h"
#include "thing.h"

class Unification
{
public:
	Unification();
};

class Unification2 : public Coro

{
public:
	Thing *x, *y;
	BnodeUnification *bnode_unification;

	Unification2(Thing *_x, Thing *_y)
		: x(_x)
		, y(_y)
	{
	}
	void *step();
};

#endif // UNIFICATION_H
