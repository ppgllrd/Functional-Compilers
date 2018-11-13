/* A stack of addresses */


#include "common.h"


#define       Max_SStack (10*1024)


/* SStackTop   points to next free item in SStack      */
/* SStackBase  points to base of current active SStack */


TAddr           SStack[Max_SStack];
TArrayIndex     SStackTopIndex  = 0;
TArrayIndex     SStackBaseIndex = 0;




