#ifndef TERM_H
#define TERM_H

#include <vector>


class Term
{
public:
    Term();
    /*
     *
     *
    Universal(local name)
    Existential(local name)
    Const(value)
    C
    c
    C
    CCCCCCcC


* */


    AtomDecls atom_decls;
};

using Terms = std::vector<Term>;





#endif // TERM_H
