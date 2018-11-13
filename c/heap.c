/* A heap */

/* ToDo:

    Cada celda debería ocupar su espacio correspondiente (no una unión)
    pero garantizando que el tamaño es >= que el tamaño de un nodo de indirección
    (para poder realizar updates en condiciones
 
    El nodo constructor no debe guardar un puntero al vector de argumentos. El
    vector de argumentos debe estar justo tras el nodo constructor siempre.

    Mejorar el movimiento de celdas con una operación del tipo move

*/

    

#include "common.h"
#include "heap.h"
#include "sstack.h"
#include <stdio.h>
#include <stdlib.h>


#define SHOWGC				FALSE



#define MAXHEAP                       	(50*1024)

/************************/
/* Nodes in the heap    */
/************************/

typedef TInteger                      TNInteger;

typedef TCharacter                    TNCharacter;

typedef struct {
          TAddr        Function;
          TAddr        Argument;
        }                             TNAp;

typedef TCode                         TNGlobal;

typedef TAddr                         TNInd;

typedef struct {
          TArity       Arity;
        }                             TVector;

typedef TVector*                      TVectorPtr;

typedef struct {
          TConstrTag   ConstrTag;
          TVectorPtr   ConstrVec;
        }                             TNConstr;

typedef TAddr                         TNForward;


typedef union {
          TNInteger    Integer;
          TNCharacter  Character;
          TNAp         Ap;
          TNGlobal     Global;
          TNInd        Ind;
          TNConstr     Constr;
          TNForward    Forward;
        }                             TContents;

typedef struct {
          TNodeTag     Tag;
          TContents    Contents;
        }                             TNode;

typedef TNode                        *TNodePtr;


/* Both spaces */
TNode Space1[MAXHEAP];
TNode Space2[MAXHEAP];

/* Pointers to begin of current from and to spaces */
TNodePtr FromSpaceBase = Space1;
TNodePtr ToSpaceBase   = Space2;

/* Pointers to first free cell in from and to spaces */
TNodePtr FromSpacePtr = Space1;
TNodePtr ToSpacePtr   = Space2;

#define FromSpaceSize  (FromSpacePtr - FromSpaceBase)


/************************/
/* Nodes allocation     */
/************************/

TAddr HeapAllocInteger (TInteger n) {
 
 (*FromSpacePtr).Tag              = TagInteger;
 (*FromSpacePtr).Contents.Integer = n;

 return (TAddr)FromSpacePtr++;
}


TAddr HeapAllocCharacter (TCharacter c) {
 
 (*FromSpacePtr).Tag                = TagCharacter;
 (*FromSpacePtr).Contents.Character = c;

 return (TAddr)FromSpacePtr++;
}


TAddr HeapAllocAp (TAddr function, TAddr argument) {
 
 (*FromSpacePtr).Tag                  = TagAp;
 (*FromSpacePtr).Contents.Ap.Function = function;
 (*FromSpacePtr).Contents.Ap.Argument = argument;

 return (TAddr)FromSpacePtr++;
}


TAddr HeapAllocGlobal (TCode code) {
 
 (*FromSpacePtr).Tag             = TagGlobal;
 (*FromSpacePtr).Contents.Global = code;

 return (TAddr)FromSpacePtr++;
}


TAddr HeapAllocInd (TAddr a) {
 
 (*FromSpacePtr).Tag          = TagInd;
 (*FromSpacePtr).Contents.Ind = a;

 return (TAddr)FromSpacePtr++;
}


TAddr HeapAllocNull () {
 
 (*FromSpacePtr).Tag = TagNull;

 return (TAddr)FromSpacePtr++;
}


TAddr HeapAllocConstr  (TConstrTag t, TAddr *as, TArity n) {
 TVectorPtr  Vptr;
 TAddrPtr    Aptr;
 TArity      i;
 TNodePtr    ret;

 

 Vptr = (TVectorPtr) (FromSpacePtr+1);

 (*FromSpacePtr).Tag                       = TagConstr;
 (*FromSpacePtr).Contents.Constr.ConstrTag = t;
 (*FromSpacePtr).Contents.Constr.ConstrVec = Vptr;

 ret = FromSpacePtr;

 (*Vptr).Arity = n;

 Aptr = (TAddrPtr) (Vptr+1);
 for(i=0;i<n;i++) {
   *Aptr = *as;
   Aptr++;
   as--;
 }

 FromSpacePtr = (TNodePtr) Aptr;

 return ret;
}



/************************/
/*Selectors		*/
/************************/

TArity NodeArity (TAddr addr) {
 TVectorPtr VPtr = (*(TNodePtr)addr).Contents.Constr.ConstrVec;
 return (*VPtr).Arity;
}


TAddrPtr NodeConstrArgs (TAddr addr) {
 TVectorPtr VPtr = (*(TNodePtr)addr).Contents.Constr.ConstrVec;
 return (TAddrPtr) (VPtr+1);
}


TInteger NodeInteger (TAddr addr)  {
  return (*(TNodePtr)addr).Contents.Integer;
}


TCharacter NodeCharacter (TAddr addr)  {
  return (*(TNodePtr)addr).Contents.Character;
}


TAddr NodeFunction (TAddr addr) {
  return (*(TNodePtr)addr).Contents.Ap.Function;
}


TAddr NodeArgument (TAddr addr) {
  return (*(TNodePtr)addr).Contents.Ap.Argument;
}


TCode NodeCode (TAddr addr) {
  return (*(TNodePtr)addr).Contents.Global;
}


TAddr NodeInd (TAddr addr)  {
  return (*(TNodePtr)addr).Contents.Ind;
}


TNodeTag  NodeTag (TAddr addr) {
  return (*(TNodePtr)addr).Tag;
}


TConstrTag NodeConstrTag (TAddr addr) {
  return (*(TNodePtr)addr).Contents.Constr.ConstrTag;
}


/************************/
/* Node update          */
/************************/

void HeapUpdate (TAddr src, TAddr dest) {
 TNodePtr NodePtr = (TNodePtr) src;

 (*NodePtr).Tag          = TagInd;
 (*NodePtr).Contents.Ind = dest;
}



/************************/
/* Garbage collection   */
/************************/


/* copy a cell to first free postition in ToSpace */
/* and returns the address of the copy            */
TNodePtr copyCell (TNodePtr ptr) {
  
  switch((*ptr).Tag) {

    case TagInteger:
    case TagForward:
    case TagCharacter: 
    case TagGlobal:   
    case TagNull:
    case TagAp:
    case TagInd: 
      *ToSpacePtr = *ptr;
      return ToSpacePtr++;

    case TagConstr: {
      TVectorPtr  VNewPtr, VPtr;
      TAddrPtr    ANewPtr, APtr;
      TArity      i, Arity;
      TNodePtr    ret;

      ret = ToSpacePtr++;

      VPtr    = (*ptr).Contents.Constr.ConstrVec;
      VNewPtr = (TVectorPtr) (ToSpacePtr+1);
      
      Arity            = (*VPtr).Arity;
      (*VNewPtr).Arity = Arity;

      APtr    = (TAddrPtr) (VPtr+1);
      ANewPtr = (TAddrPtr) (VNewPtr+1);
      
      for(i=0;i<Arity;i++) {
        *ANewPtr = *APtr;
        ANewPtr++;
        APtr++;
      }

      ToSpacePtr = (TNodePtr) ANewPtr;
      
      (*ret).Tag                       = TagConstr;
      (*ret).Contents.Constr.ConstrTag = (*ptr).Contents.Constr.ConstrTag;
      (*ret).Contents.Constr.ConstrVec = VNewPtr;

      return ret;
    }

    default:
      printf("copy: unkown tag");
      exit(1);  
  }
}


TNodePtr evacuate (TNodePtr ptr) {
  TNodePtr toPtr;
  
  switch((*ptr).Tag) {
  
    case TagForward:
      return (*ptr).Contents.Forward;

    case TagInteger:
    case TagCharacter: 
    case TagGlobal:   
    case TagNull:
    case TagAp:
    case TagInd:
    case TagConstr: 
      toPtr = copyCell (ptr);
      (*ptr).Tag              = TagForward;
      (*ptr).Contents.Forward = toPtr;
      return toPtr;

    default:
      printf("evacuate: unknown tag");
      exit(1);  
  }    
}


clearSpace (TNodePtr ptr) {
  int i;
  for (i=0; i<MAXHEAP; i++) {
    (*ptr).Tag = TagNull;
    (*ptr).Contents.Ap.Function = (TNodePtr) 0;
    (*ptr).Contents.Ap.Argument = (TNodePtr) 0;
    ptr++;
  }  
}        

         
void gc (void) {
  TArrayIndex  i;
  TNodePtr     scavePtr, Swap;
  int          initialSize, finalSize;

#if SHOWGC
  initialSize = FromSpaceSize;
#endif
  
  for(i=0;i<SStackTopIndex;i++) {
    SStackSelect(i) = (TAddr) evacuate((TNodePtr)SStackSelect(i));    
  }

  scavePtr = ToSpaceBase;
  
  while (scavePtr < ToSpacePtr) {
  
    switch((*scavePtr).Tag) { 
  
      case TagInteger:
      case TagCharacter: 
      case TagGlobal:   
      case TagNull:
        
        scavePtr++;      
        break;
        
      case TagInd:
        (*scavePtr).Contents.Ind = evacuate((*scavePtr).Contents.Ind); 
        scavePtr++;
        break;
      
      case TagAp:
      
        (*scavePtr).Contents.Ap.Function = evacuate((*scavePtr).Contents.Ap.Function);
        (*scavePtr).Contents.Ap.Argument = evacuate((*scavePtr).Contents.Ap.Argument);
        scavePtr++;
        break;
      
      case TagConstr: {
        TVectorPtr VPtr;
        TAddrPtr   APtr;        
        TArity     i, Arity;        
        
        VPtr    = (*scavePtr).Contents.Constr.ConstrVec;
        Arity   = (*VPtr).Arity;
        APtr    = (TAddrPtr) (VPtr+1);
        
        for(i=0;i<Arity;i++) {
          *APtr = evacuate(*APtr);
          APtr++;
        }
        
        scavePtr = (TNodePtr)APtr;
        break;
        
      }

      case TagForward:
        printf("Error: scave Forward cell");
        exit(1);

      default:
        printf("scave: unknown tag");
        exit(1);  
      
    }  
  }
    

  Swap          = FromSpaceBase;
  FromSpaceBase = ToSpaceBase;
  ToSpaceBase   = Swap;
  
  FromSpacePtr = ToSpacePtr;
  ToSpacePtr   = ToSpaceBase;  
  
/*  clearSpace (ToSpaceBase);*/
  
#if SHOWGC  
  finalSize = FromSpaceSize;

  printf(" <GC recovered %d> ",initialSize-finalSize);
#endif
}


#define FULLHEAP  (MAXHEAP-5*1024)

void CheckGC (void) {
 if (FromSpaceSize > FULLHEAP)
   gc();
 if (FromSpaceSize > FULLHEAP) {
   printf("Heap exhausted");
   exit(1);
 }
}
