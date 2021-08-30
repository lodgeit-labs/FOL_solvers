#ifndef BNDATA_H
#define BNDATA_H

#include "thing.h"
#include <stddef.h>

class Bndata
{
public:
	Bndata();
	size_t rule;
	Thing *things;
};

#endif // BNDATA_H
