#ifndef CONSTMANAGER_H
#define CONSTMANAGER_H

#include "const.h"
#include <vector>

class ConstManager
{
public:
	ConstManager();
	std::vector<*Const> consts;
	ConstRef add(Const &c);
};

#endif // CONSTMANAGER_H
