#ifndef THING_H
#define THING_H

#include "const.h"
#include <QString>

enum ThingType
{
	EXISTENTIAL,
	UNIVERSAL,
	CONST
};

class Thing
{
public:
	ThingType type;
	union
	{
		Thing *binding;
		ConstRef c;
	};
	Thing();
	bool eq(const Thing &rhs)
	{
		return type == rhs.type && binding == rhs.binding;
	}
	QString str()
	{
		return QString("thing");
	}
	void bind(Thing *y)
	{
		binding = y;
	}
	void unbind()
	{
		binding = nullptr;
	}
};

#endif // THING_H
