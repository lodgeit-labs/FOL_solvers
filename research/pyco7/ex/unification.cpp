#include "unification.h"
#include <QString>
#include <QtGlobal>

Unification::Unification() {}

#define yield(label, _reason)           \
	{                                   \
		entry_str = QString("" #label); \
		reason = QString(_reason);      \
		entry = &&label;                \
		return entry;                   \
	}

#define quit(_reason)                \
	{                                \
		entry_str = QString("quit"); \
		reason = QString(_reason);   \
		entry = nullptr;             \
		return entry;                \
	}

void *Unification2::step()
{
	if (entry)
		goto *entry;
	if (x == y || x->eq(*y))
	{
		yield(end, "same things")
	}
	if (x->type == UNIVERSAL)
	{
		Q_ASSERT(x->binding == nullptr);
		x->bind(y);
		// reason =
		yield(unbind_x, "x bound to y")
	}
	if (y->type == UNIVERSAL)
	{
		Q_ASSERT(y->binding == nullptr);
		y->bind(x);
		yield(unbind_y, "y bound to x")
	}
	if (y->type == y->type)
	{
		if (x->type == CONST)
		{
			if (x->c == y->c)
			{
				yield(end, "same consts");
			}
			else
			{
				quit(QString("different consts: %1 vs %2").arg(x->c).arg(y->c))
			}
		}
		if (x->type == EXISTENTIAL)
		{
			bnode_unification = new BnodeUnification(x, y);
			goto l_bnode_unification;
		}
	}
	quit(QString("different things: %1 vs %2").arg(x->str(), y->str()));
end:
	quit("done");
l_bnode_unification:;
	{
		if (bnode_unification->step())
		{
			yield(l_bnode_unification, "bnodes unified");
		}
		delete bnode_unification;
		quit("no more ways to unify bnodes");
	}
unbind_x:;
	{
		x->unbind();
		quit("done");
	}
unbind_y:;
	{
		y->unbind();
		quit("done");
	}
}
