#ifndef RULE_H
#define RULE_H

#include "term.h"
#include <QString>

class Rule
{
public:
    Rule();
    Terms head;
    Terms body;
    long id;
    QString uri;
};

#endif // RULE_H
