#ifndef BNODEUNIFICATION_H
#define BNODEUNIFICATION_H

#include "coro.h"
#include "thing.h"

class BnodeUnification : public Coro
{
public:
BnodeUnification(Thing* _x_addr, Thing* _y_addr);
};

#endif // BNODEUNIFICATION_H
