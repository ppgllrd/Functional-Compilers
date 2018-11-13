-------------------------------------------------------
-- A Compiler that generates C code
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------
 
module CCompiler (
         ccompile
       ) where


import GlobalDefs
import LibPretty
import LibAssoc
import CoreLex
import GMState
import GMGlobals
import GMHeap
import GMCode


operators :: Assoc Name Name
operators = aFromList [ (tkAdd, "primIntAdd"),
                        (tkSub, "primIntSub"),
                        (tkMul, "primIntMul"),
                        (tkDiv, "primIntDiv"),
                        
                        (tkLT,  "primIntLT"),
                        (tkLE,  "primIntLE"),
                        (tkEQT, "primIntEQT"),
                        (tkNEQT,"primIntNEQT"),
                        (tkGT,  "primIntGT"),  
                        (tkGE,  "primIntGE"),
                        
                        (tkOr,  "primBoolOr"),                        
                        (tkAnd, "primBoolAnd")                        
                        ]

iName :: Name -> Iseq                        
iName name = case aLookup operators name of
               Just name' -> iStr ("_"++name')
               Nothing    -> iStr ("_"++name)

ccompile :: GMState -> String
ccompile state = iDisplay (iConcat [iDecls state, iNewline, iNewline, iDefns state])

iDecls :: GMState -> Iseq
iDecls state = iConcat [ 
                 iStr "/* Supercombinators declarations */", iNewline,
                 iConcat ( map iDecl (globalToList (gmGlobals state)) )
               ]   

iDecl :: (Name, Addr) -> Iseq
iDecl (name, addr) = iConcat [iStr "ScDecl(", iName name, iStr ")", iNewline]

iDefns :: GMState -> Iseq
iDefns state = iConcat [ 
                    iStr "/* Supercombinators definitions */", iNewline,
                    iConcat ( map (iDefn state) (globalToList (gmGlobals state)) ),
                    iNewline, iNewline
                 ]   
                 
iDefn :: GMState -> (Name, Addr) -> Iseq
iDefn state (name, addr) 
   = iConcat [ 
       iStr "ScDefn(", iName name, iStr ")", 
       iInstructions code arity, iNewline, iNewline
     ]
     where Just (NGlobal arity code) = heapLookup (gmHeap state) addr

iInstructions :: GMCode -> Int -> Iseq
iInstructions code arity
   = iConcat [
       iStr "{", iNewline, iStr "  ",
       iIndent (iConcat [iRearrange,
                         iInterleave iNewline (map iInstruction is)
                ]),
       iNewline, 
       iStr "}"
     ]
   where is         = codeToList code
         iRearrange = if arity==0 then iNil 
                                  else iConcat [iStr "Rearrange(",
                                                iInt arity, iStr ")", 
                                                iNewline]        
   
iCode :: GMCode -> Iseq
iCode code = iInstructions code 0   
   
iInstruction :: Instruction -> Iseq
iInstruction (Unwind)             = iStr "Unwind"
iInstruction (Pushglobal name)    = iConcat [iStr "Pushglobal(", iName name, iStr ")"]
iInstruction (Pushint int)	  = iConcat [iStr "Pushint(", iInt int, iStr ")"]
iInstruction (Pushchar char)	  = iConcat [iStr "Pushchar(", iStr (show char), iStr ")"]
iInstruction (Push int)	          = iConcat [iStr "Push(", iInt int, iStr ")"]
iInstruction (Mkap)		  = iStr "Mkap"
iInstruction (Update int)	  = iConcat [iStr "Update(", iInt int, iStr ")"]
iInstruction (Pop int)	          = iConcat [iStr "Pop(", iInt int, iStr ")"]
iInstruction (Slide int)	  = iConcat [iStr "Slide(", iInt int, iStr ")"]
iInstruction (Alloc int)	  = iConcat [iStr "Alloc(", iInt int, iStr ")"]
iInstruction (Eval)	          = iStr "Eval"
iInstruction (IAdd)	          = iStr "IAdd"
iInstruction (ISub)	          = iStr "ISub"
iInstruction (IMul)	          = iStr "IMul"
iInstruction (IDiv)	          = iStr "IDiv"
iInstruction (INegate)	          = iStr "INegate"
iInstruction (IEQT)	          = iStr "IEQT"
iInstruction (INEQT)	          = iStr "INEQT"
iInstruction (ILT)	          = iStr "ILT"
iInstruction (ILE)	          = iStr "ILE"
iInstruction (IGT)	          = iStr "IGT"
iInstruction (IGE)	          = iStr "IGE"
iInstruction (COrd)	          = iStr "COrd"
iInstruction (CChr)	          = iStr "CChr"
iInstruction (Cond c1 c2)	  = iConcat [iStr "BeginCond", iNewline, iStr "  ", 
                                             iIndent (iConcat [iStr "BeginLabel(TagTrue) ", iNewline, iCode c1, iNewline, iStr "EndLabel", iNewline,
                                                               iStr "BeginLabel(TagFalse)", iNewline, iCode c2, iNewline, iStr "EndLabel" 
                                                      ]), iNewline,
                                             iStr "EndCond"
                                    ]
iInstruction (Pack n1 n2)         = iConcat [iStr "Pack(", iInt n1, iStr ",", iInt n2, iStr ")"]
iInstruction (Casejump xs)        = iConcat [iStr "BeginCasejump", iNewline, iStr "  ", iIndent (iConcat (map showCase xs)), iNewline, iStr "EndCasejump"]
				      where showCase (n, code) = iConcat [iStr "BeginLabel(",iInt n, iStr ") ", iCode code,iNewline,iStr "EndLabel",iNewline]
iInstruction (Split n)            = iConcat [iStr "Split(", iInt n, iStr ")"]
iInstruction (Print)	          = iStr "Print"
iInstruction (PrintStr str)       = iConcat [iStr "Print(", iStr str, iStr ")"]
iInstruction (Mkbool)             = iStr "Mkbool"
iInstruction (Mkint)              = iStr "Mkint"
iInstruction (Mkchar)             = iStr "Mkchar"
iInstruction (Get)                = iStr "Get"
iInstruction (Pushbasic v)        = iConcat [iStr "Pushbasic(", iBasic v, iStr ")"]
                                    where iBasic (BasicChar c) = iStr ("BasicChar("++show c++")")
                                          iBasic (BasicInt n)  = iStr ("BasicInt("++show n++")")
iInstruction (Return)             = iStr "Return"
   
   

{-
--------------------------------------------------------
-- showing the Machine state
--------------------------------------------------------

showSCs :: Monadic ()
showSCs = do state <- readST
             (putStr . iDisplay . iSCs) state


showState :: Monadic ()
showState = do state <- readST
               (putStr . iDisplay . iState) state

showStats :: Monadic ()
showStats = do state <- readST
               (putStr . iDisplay . iStats) state



iSCs :: GMState -> Iseq
iSCs state
   = iConcat [
       iStr "Supercombinator definitions", iNewline,
       iInterleave iNewline (map (iSC state) (globalToList (gmGlobals state))),
       iNewline, iNewline
     ]



iSC :: GMState -> (Name, Addr) -> Iseq
iSC s (name, addr)
   = iConcat [ 
       iStr "Code for ", iStr name, iNewline,
       iInstructions code, iNewline, iNewline
     ]
     where Just (NGlobal arity code) = heapLookup (gmHeap s) addr

iInstructions :: GMCode -> Iseq
iInstructions code
   = iConcat [
       iStr " Code:  {",
       iIndent (iInterleave iNewline (map toIseq is)),
       iStr "}", iNewline
     ]
   where is = codeToList code


iState :: GMState -> Iseq
iState s
   = iConcat [ 
       iNewline,
       iSStack s,                iNewline,
       iVStack s,                iNewline,
       iDump s,                  iNewline,
       
       iInstructions (gmCode s), iNewline 
     ]

iSStack :: GMState -> Iseq
iSStack s
   = iConcat [
       iStr " SStack:[",
       iIndent (iInterleave iNewline
                       (map (iSStackItem s) ((reverse.sstackToList.gmSStack) s))),
       iStr "]"
     ]

iSStackItem :: GMState -> Addr -> Iseq
iSStackItem s a
   = iConcat [
       iStr (showAddr a), iStr ": ",
       iNode s a node
     ]
     where Just node = heapLookup (gmHeap s) a

iVStack :: GMState -> Iseq
iVStack s
   = iConcat [iStr " VStack:[",
              iInterleave (iStr ", ") (map (iStr.show) ((reverse.vstackToList.gmVStack) s)),
              iStr "]"
     ]         
   

iNode :: GMState -> Addr -> Node -> Iseq
iNode s a (NInt n)       = iInt n
iNode s a (NChar c)      = iStr (show c)
iNode s a (NGlobal n g)  = iConcat [iStr "Global ", iStr v]
                              where Just v = globalLookupFromAddr (gmGlobals s) a

iNode s a (NAp a1 a2)    = iConcat [
                             iStr "Ap ", iStr (showAddr a1),
                             iStr " ",   iStr (showAddr a2)
                           ]
iNode s a (NInd a1)      = iConcat [
                             iStr "Ind ", iStr (showAddr a1)
                           ]
iNode s a (NConstr t as) = iConcat [
                            iStr "Constr ", iInt t, iStr " [",
                            iInterleave (iStr ", ") (map (iStr.showAddr) as), iStr "]"
                          ]

iStats :: GMState -> Iseq
iStats s
   = iConcat [
       iNewline,
       iStr "Steps taken = ", iInt (statGetSteps (gmStats s)), iNewline,
       iStr "Heap nodes allocated = ", iInt (statGetHeaps (gmStats s)),iNewline,
       iStr "Maximum sstack length = ", iInt (statGetSStacks (gmStats s)),iNewline,
       iStr "Maximum vstack length = ", iInt (statGetVStacks (gmStats s)),iNewline,
       iStr "Maximum dump length = ", iInt (statGetDumps (gmStats s)),

       iNewline
     ]


iDump :: GMState -> Iseq
iDump s
   = iConcat [ iStr " Dump  :[",
               iIndent (iInterleave iNewline
                        (map iDumpItem (reverse (dumpToList (gmDump s))))),
               iStr "]"
     ]

iDumpItem :: (GMCode, GMSStack) -> Iseq
iDumpItem (code, sstack)
    = iConcat [ iStr "<",
                iShortCode 3 code, iStr ", ",
                iShortSStack sstack,
                iStr ">"
      ]

iShortSStack :: GMSStack -> Iseq
iShortSStack sstack 
     = iConcat [ iStr "[",
                 iInterleave (iStr ", ") (map (iStr . showAddr) (sstackToList sstack)),
                 iStr "]"
       ]

-}