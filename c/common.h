/* Common definitions */


#ifndef COMMON_H

#define COMMON_H   1

#define FALSE           0
#define TRUE            1
typedef short int       TBool;


typedef void           *TVoidPtr;
typedef TVoidPtr        TAddr;
typedef TVoidPtr        TCode;
typedef TAddr          *TAddrPtr;

typedef int             TInteger;
typedef char            TCharacter;

typedef union {
  TInteger   Integer;
  TCharacter Character;
}                       TBasic;

typedef int             TArrayIndex;

typedef int             TArity;

typedef short int       TNodeTag;

typedef int             TConstrTag;

#endif /* ifndef */
