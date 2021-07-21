#ifndef THING_H
#define THING_H



enum ThingType {BOUND_EXISTENTIAL, UNBOUND_EXISTENTIAL, BOUND_UNIVERSAL, UNBOUND_UNIVERSAL, CONST, OFFSET};


Class Thing {
public:
   //Structure
   ThingType type;
   union {
       //maybe call this value? how about binding?
       //both sound appropriate; better than thing anyway
       Thing *thing;     // for bound var
       nodeid node;      // for node
       size_t size;      // for list. not sure
       offset_t offset;
   };
}


class Thing
{
public:
    Thing();
    bool operator == (const Thing& lhs, const Thing& rhs) {
        return lhs.type == rhs.type &&
                lhs.thing = rhs.thing;
    }
};

#endif // THING_H
