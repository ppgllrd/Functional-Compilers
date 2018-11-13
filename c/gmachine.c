#include "common.h"
#include "sstack.h"
#include "vstack.h"
#include "dump.h"
#include "heap.h"

#include <stdio.h>
#include <stdlib.h>


#define INLINE_UNWIND                   TRUE


/* Macros */

#define ScDefn(X)                       void X (void)
#define ScDecl(X)                       ScDefn(X);
#define ProgramIP(X)                    TVoidPtr (*(X))(void);



/**************************/
/* G-Machine Instructions */
/**************************/

#define TagFalse                        1
#define TagTrue                         2


#define Pushglobal(f) {                                  \
  TAddr a = HeapAllocGlobal((f));                        \
  SStackPush(a);                                         \
  CheckGC();                                             \
}


#define Pushint(n) {                                     \
  TAddr a = HeapAllocInteger((n));                       \
  SStackPush(a);                                         \
  CheckGC();                                             \
}


#define Pushchar(c) {                                    \
  TAddr a = HeapAllocCharacter((c));                     \
  SStackPush(a);                                         \
  CheckGC();                                             \
}


#define Push(n) {                                        \
  TAddr an = SStackSelect((n));                          \
  SStackPush(an);                                        \
}


#define Mkap {                                           \
  TAddr a0 = SStackPop();                                \
  TAddr a1 = SStackPop();                                \
  TAddr a  = HeapAllocAp(a0,a1);                         \
  SStackPush(a);                                         \
  CheckGC();                                             \
}


#define Update(n) {                                      \
  TAddr  a  = SStackPop();                               \
  TAddr  an = SStackSelect((n));                         \
  HeapUpdate(an, a);                                     \
}


#define Pop(n) {                                         \
 SStackDrop(n);                                          \
}


#define Rearrange(n) {                                   \
  int k = SStackLength()-1;                              \
  if (k<(n)) {                                           \
   TAddr ak = SStackSelect((k));                         \
   DumpRestore();                                        \
   SStackPush(ak);                                       \
   return;                                               \
  }                                                      \
  else {                                                 \
   int i;                                                \
   for(i=0;i<n;i++) {                                    \
     SStackUpdate(i, NodeArgument(SStackSelect((i+1)))); \
   }                                                     \
  }                                                      \
}                                                        \


#define InlineUnwind {                                   \
  int   ExitUnwind = FALSE;                              \
  TAddr a;                                               \
  while(!ExitUnwind) {                                   \
    a = SStackTop();                                     \
    switch(NodeTag(a)) {                                 \
      case TagInteger   :                                \
      case TagCharacter :                                \
      case TagConstr    : {                              \
         DumpRestore();                                  \
         SStackPush(a);                                  \
         ExitUnwind=TRUE;                                \
         break;                                          \
      }                                                  \
      case TagAp: {                                      \
         TAddr a1 = NodeFunction(a);                     \
         SStackPush(a1);                                 \
         break;                                          \
      }                                                  \
      case TagInd: {                                     \
         TAddr ai = NodeInd(a);                          \
         SStackDrop(1);                                  \
         SStackPush(ai);                                 \
         break;                                          \
      }                                                  \
      case TagGlobal: {                                  \
         ProgramIP(ip);                                  \
         ip = NodeCode(a);                               \
         (*ip)();                                        \
         ExitUnwind=TRUE;                                \
         break;                                          \
      }                                                  \
    }                                                    \
  }                                                      \
}


#if INLINE_UNWIND
#define Unwind          InlineUnwind
#else
void PrimUnwind(void) {
  InlineUnwind
}
#define Unwind          PrimUnwind();
#endif

#define Slide(n) {                                       \
  TAddr a0 = SStackTop();                                \
  SStackDrop(n);                                         \
  SStackPush(a0);                                        \
}


#define Alloc(n) {                                       \
  int   i;                                               \
  TAddr a;                                               \
  for(i=0;i<n;i++) {                                     \
    a = HeapAllocNull();                                 \
    SStackPush(a);                                       \
  }                                                      \
  CheckGC();                                             \
}


#define Pack(t,n) {                                      \
  TAddr a;                                               \
  a = HeapAllocConstr(t,SStackTopAddr(),n);              \
  SStackDrop(n);                                         \
  SStackPush(a);                                         \
  CheckGC();                                             \
}


#define Split(n) {                                       \
  TAddr    a    = SStackPop();                           \
  TAddrPtr args = NodeConstrArgs(a);                     \
  TArity   i;                                            \
  for(i=NodeArity(a)-1;i>=0;i--) {                       \
    SStackPush(args[i]);                                 \
  }                                                      \
}


#define Eval {                                           \
  TAddr a = SStackPop();                                 \
  DumpSave();                                            \
  SStackMakeEmpty();                                     \
  SStackPush(a);                                         \
  Unwind;                                                \
}


#define PrintStr(X)             fputs((X),stdout)
#define PrintInteger(X)         printf("%d",(X))
#define PrintCharacter(X)       putchar((X))
#define PrintConstrTag          PrintInteger

void PrimPrint(void) {
  TAddr    a = SStackTop();
  int      n;
  TAddrPtr APtr;
  TArity   Arity;

  switch (NodeTag(a)) {
   case TagInteger:   PrintInteger(NodeInteger(a));
                      break;
   case TagCharacter: PrintCharacter(NodeCharacter(a));
                      break;
   case TagConstr:    PrintCharacter('C');
                      PrintConstrTag(NodeConstrTag(a));
                      PrintCharacter('{');
                      Arity = NodeArity(a);
                      for (n=0;n<Arity;n++) {
			/* Cell "a" may have moved during GC */
			APtr  = NodeConstrArgs(a);
                        SStackPush(APtr[n]);
                        Eval;
                        PrimPrint();
                        PrintCharacter(' ');
                      }
                      PrintCharacter('}');
                      break;
   case TagAp:        PrintStr("NAp");
                      break;
   case TagGlobal:    PrintStr("NGlobal");
                      break;
   case TagInd:       PrintStr("NInd");
                      break;
  }
}

#define Print PrimPrint();


#define Mkint {                                          \
  TBasic x = VStackPop();                                \
  TAddr a  = HeapAllocInteger(x.Integer);                \
  SStackPush(a);                                         \
  CheckGC();                                             \
}


#define Mkbool {                                         \
  TBasic x = VStackPop();                                \
  TAddr a  = HeapAllocConstr(x.Integer,NULL,0);          \
  SStackPush(a);                                         \
  CheckGC();                                             \
}


#define Mkchar {                                         \
  TBasic x = VStackPop();                                \
  TAddr a  = HeapAllocCharacter(x.Character);            \
  SStackPush(a);                                         \
  CheckGC();                                             \
}


#define Get {                                            \
  TAddr a = SStackPop();                                 \
  while(NodeTag(a)==TagInd) {                            \
    a = NodeInd(a);                                      \
  }                                                      \
  switch(NodeTag(a)) {                                   \
    case TagInteger: {                                   \
      TBasic n;                                          \
      n.Integer = NodeInteger(a);                        \
      VStackPush(n);                                     \
      break;                                             \
    }                                                    \
    case TagCharacter: {                                 \
      TBasic n;                                          \
      n.Character = NodeCharacter(a);                    \
      VStackPush(n);                                     \
      break;                                             \
    }                                                    \
    case TagConstr: {                                    \
      TBasic n;                                          \
      n.Integer = NodeConstrTag(a);                      \
      VStackPush(n);                                     \
      break;                                             \
    }                                                    \
  }                                                      \
}


#define Primitive2(op) {                                 \
  TBasic b1 = VStackPop();                               \
  TBasic b2 = VStackPop();                               \
  TBasic res;                                            \
  res.Integer = b1.Integer op b2.Integer;                \
  VStackPush(res);                                       \
}

#define Primitive1(op) {                                 \
  TBasic b1 = VStackPop();                               \
  TBasic res;                                            \
  res.Integer = op(b1.Integer);                          \
  VStackPush(res);                                       \
}

#define BoolToInteger(x)                                 \
  ((x)==TRUE ? TagTrue : TagFalse)

#define Comparison(op) {                                 \
  TBasic b1 = VStackPop();                               \
  TBasic b2 = VStackPop();                               \
  TBasic res;                                            \
  res.Integer = BoolToInteger((b1.Integer)               \
                              op                         \
                              (b2.Integer));             \
  VStackPush(res);                                       \
}

#define IAdd            Primitive2(+)
#define ISub            Primitive2(-)
#define IMul            Primitive2(*)
#define IDiv            Primitive2(/)

#define INegate         Primitive1(-)

#define IEQT            Comparison(==)
#define INEQT           Comparison(!=)
#define ILT             Comparison(<)
#define ILE             Comparison(<=)
#define IGT             Comparison(>)
#define IGE             Comparison(>=)

#define CChr {                                           \
  TBasic b1 = VStackPop();                               \
  TBasic res;                                            \
  res.Character = (TCharacter)(b1.Integer);              \
  VStackPush(res);                                       \
}


#define COrd {                                           \
  TBasic b1 = VStackPop();                               \
  TBasic res;                                            \
  res.Integer = (TInteger)(b1.Character);                \
  VStackPush(res);                                       \
}


/* Implementation of pushbasic */

TBasic TempBasic;

#define BasicInt(x) {                                    \
  TempBasic.Integer = x;                                 \
}

#define BasicChar(x) {                                   \
  TempBasic.Character = x;                               \
}

#define Pushbasic(x) {                                   \
  x;                                                     \
  VStackPush(TempBasic);                                 \
}


/* Implementation of Cond */

#define BeginCond {                                      \
  TBasic x = VStackPop();                                \
  switch(x.Integer) {

#define EndCond   }}

#define BeginLabel(x)   case (x):
#define EndLabel        break;




/* Implementation of CaseJump */

#define BeginCasejump {                                  \
  TAddr a = SStackTop();                                 \
  switch(NodeConstrTag(a)) {

#define EndCasejump  }}


#define Return  {					 \
  printf("Error: Return non implemented");		 \
  exit(1);						 \
}



/* Declarar todos los supercombinadores */
/* Debe ir a un fichero .h              */


/* Cuerpo de los supercombinadores */
#include "out.c"


/********************/
/* Main entry point */
/********************/

main () {

 Pushglobal(_main);
 Eval;
 printf("\n\nResult ");
 Print
 return 0;
}
