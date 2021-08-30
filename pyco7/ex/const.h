#ifndef CONST_H
#define CONST_H

#include <stddef.h>

typedef size_t ConstRef;

class Const
{
public:
	Const(QString string);
	QString string;
};

#endif // CONST_H
