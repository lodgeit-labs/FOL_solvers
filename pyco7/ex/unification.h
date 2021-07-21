#ifndef UNIFICATION_H
#define UNIFICATION_H

#include "coro.h"
#include "thing.h"

class Unification
{
public:
Unification();
};



class Unification2: public Coro
{
public:
Thing *x_addr;
Thing *y_addr;
Unification2(Thing *_x_addr, Thing *_y_addr):x_addr(_x_addr),y_addr(_y_addr){}
void step();
};


#endif // UNIFICATION_H
