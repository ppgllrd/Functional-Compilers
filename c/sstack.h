#ifndef SSTACK_H

#define SSTACK_H     1

#include "common.h"

extern TAddr           SStack[];
extern TArrayIndex     SStackTopIndex;
extern TArrayIndex     SStackBaseIndex;


#define SStackPush(x)       SStack[SStackTopIndex++] = (x)
#define SStackPop()         SStack[--SStackTopIndex]
#define SStackTop()         SStack[SStackTopIndex-1]
#define SStackSelect(n)     SStack[SStackTopIndex-(1+(n))]
#define SStackDrop(n)       SStackTopIndex -= (n)
#define SStackUpdate(n,x)   SStack[SStackTopIndex-(1+(n))] = (x)
#define SStackTopAddr()     (TAddrPtr)&(SStack[SStackTopIndex-1])

#define SStackLength()      (SStackTopIndex-SStackBaseIndex)
#define SStackMakeEmpty()   SStackBaseIndex=SStackTopIndex


#endif /* ifndef */

