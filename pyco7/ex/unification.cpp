#include "unification.h"

Unification::Unification()
{

}

void Unification2::step()
{
switch(entry)
{
0:
{
Thing x = *x_addr;
Thing y = *y_addr;

if (x == y)
return 1000000;
}
{
if (x.type == UNBOUND_UNIVERSAL)
{
x.bind(y);
return 10;
}
x.unbind();
}
else
{
if (y.type == UNBOUND_UNIVERSAL)
{
y.bind(x);
co_yield;
y.unbind();
}


}

}

}
*/
100:
return -1;

}
}
