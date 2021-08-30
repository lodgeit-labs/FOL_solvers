#include "constmanager.h"

ConstManager::ConstManager()
{
}

ConstRef ConstManager::add(Const &c)
{
	ConstRef i = idx(c);
	if (i)
		return i;
	consts.push(c);
	i = consts.size();
	return i;
}

ConstRef ConstManager::idx(Const &c)
{
	for (size_t i = 0; i < consts.size(); i++)
	{
		Const *item = consts[i];
		if (item->string == c.string)
			return i + 1;
	}
	return 0; //not found
}
