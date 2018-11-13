#ifndef DUMP_H


#define DUMP_H     1

#include "common.h"


typedef struct {
  TArrayIndex BaseIndex;
  TArrayIndex TopIndex;
} TDumpItem;


extern TDumpItem         Dump[];
extern TArrayIndex       DumpTopIndex;


#define DumpSave() {                               \
  Dump[DumpTopIndex].BaseIndex = SStackBaseIndex;  \
  Dump[DumpTopIndex].TopIndex  = SStackTopIndex;   \
  DumpTopIndex++;                                  \
}

#define DumpRestore() {                            \
  DumpTopIndex--;                                  \
  SStackBaseIndex = Dump[DumpTopIndex].BaseIndex;  \
  SStackTopIndex  = Dump[DumpTopIndex].TopIndex;   \
}


#endif /* ifndef */

