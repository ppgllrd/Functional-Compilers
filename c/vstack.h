#ifndef VSTACK_H

#define VSTACK_H      1

#include "common.h"

extern TBasic              VStack[];
extern TArrayIndex         VStackTopIndex;

#define VStackPush(x)      VStack[VStackTopIndex++] = (x)
#define VStackPop()        VStack[--VStackTopIndex]
#define VStackTop()        VStack[VStackTopIndex-1]
#define VStackSelect(n)    VStack[VStackTopIndex-(1+(n))]
#define VStackDrop(n)      VStackPtr -= (n)
#define VStackUpdate(n,x)  VStack[VStackTopIndex-(1+(n))] = (x)

#endif /* ifndef */

