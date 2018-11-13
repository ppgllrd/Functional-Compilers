/* A stack of Basic values */

#include "vstack.h"

#define Max_VStack 5*1024

TBasic          VStack[Max_VStack];
TArrayIndex     VStackTopIndex = 0;


