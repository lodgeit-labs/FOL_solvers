#ifndef BNODEUNIFICATIONO_H
#define BNODEUNIFICATIONO_H

#include "coro.h"
#include "thing.h"

class BnodeUnificationo : public Coro {
public:
	BnodeUnificationo(Thing* _x_addr, Thing* _y_addr);
};

#endif // BNODEUNIFICATIONO_H
