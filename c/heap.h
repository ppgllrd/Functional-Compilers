#ifndef  HEAP_H

#define HEAP_H   1

#include "common.h"


#define TagInteger                0
#define TagCharacter              1
#define TagAp                     2
#define TagGlobal                 3
#define TagInd                    4
#define TagConstr                 5
#define TagNull                   6
#define TagForward                7 



TAddr         HeapAllocInteger    (TInteger n);
TAddr         HeapAllocCharacter  (TCharacter c);
TAddr         HeapAllocAp         (TAddr function, TAddr argument);
TAddr         HeapAllocGlobal     (TCode code);
TAddr         HeapAllocInd        (TAddr addr);
TAddr         HeapAllocConstr     (TConstrTag t, TAddr *as, TArity n);
TAddr         HeapAllocNull       ();


void          HeapUpdate          (TAddr src, TAddr dest);


TNodeTag      NodeTag             (TAddr addr);


TInteger      NodeInteger         (TAddr addr);
TCharacter    NodeCharacter       (TAddr addr);
TAddr         NodeFunction        (TAddr addr);
TAddr         NodeArgument        (TAddr addr);
TCode         NodeCode            (TAddr addr);
TAddr         NodeInd             (TAddr addr);
TConstrTag    NodeConstrTag       (TAddr addr);
TArity        NodeArity           (TAddr addr);
TAddrPtr      NodeConstrArgs      (TAddr addr);

void CheckGC (void);

#endif /* ifndef */
