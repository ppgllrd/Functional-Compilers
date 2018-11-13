-------------------------------------------------------
-- Mark 1 Abstract Machine
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------
 
module GMachine (
         eval, trace
       ) where


import Prelude hiding (catch, fail, putStr, print)
import Char
import LibSIO
import Monad hiding (fail)

import LibPretty
import CoreLex
import LibAssoc
import GlobalDefs
import GMCode
import GMSStack
import GMVStack
import GMHeap
import GMGlobals
import GMStats
import GMState
import GMDump
import ConstrTags

--------------------------------------------------------
-- The G-Machine
--------------------------------------------------------


type Monadic a = SIO GMState a

raiseError :: String -> Monadic a
raiseError err = fail (userError err)


getCode :: Monadic GMCode
getCode = do st <- readST
             return (gmCode st)

putCode :: GMCode -> Monadic ()
putCode gmCode' = do st <- readST
                     let newST = st { gmCode = gmCode' }
                     writeST newST

getSStack :: Monadic GMSStack
getSStack = do st <- readST
               return (gmSStack st)

putSStack :: GMSStack -> Monadic ()
putSStack gmSStack' = do st <- readST
                         let newST = st { gmSStack = gmSStack' }
                         writeST newST

getVStack :: Monadic GMVStack
getVStack = do st <- readST
               return (gmVStack st)

putVStack :: GMVStack -> Monadic ()
putVStack gmVStack' = do st <- readST
                         let newST = st { gmVStack = gmVStack' }
                         writeST newST

getHeap :: Monadic GMHeap
getHeap = do st <- readST
             return (gmHeap st)

putHeap :: GMHeap -> Monadic ()
putHeap gmHeap' = do st <- readST
                     let newST = st { gmHeap = gmHeap' }
                     writeST newST

getGlobals :: Monadic GMGlobals
getGlobals = do st <- readST
                return (gmGlobals st)

putGlobals :: GMGlobals -> Monadic ()
putGlobals gmGlobals' = do st <- readST
                           let newST = st { gmGlobals = gmGlobals' }
                           writeST newST
 
getStats :: Monadic GMStats
getStats = do st <- readST
              return (gmStats st)

putStats :: GMStats -> Monadic ()
putStats gmStats' = do st <- readST
                       let newST = st { gmStats = gmStats' }
                       writeST newST

getDump :: Monadic GMDump
getDump = do st <- readST
             return (gmDump st)

putDump :: GMDump -> Monadic ()
putDump gmDump' = do st <- readST
                     let newST = st { gmDump = gmDump' }
                     writeST newST


putOutput :: String -> Monadic ()
putOutput str = do putStr str


step :: Monadic ()
step = do code <- getCode
          let (i:is) = codeToList code
          putCode (codeFromList is)
          dispatch i

doAdmin :: Monadic ()
doAdmin = do stats <- getStats
             let newStats = statIncSteps stats
             putStats newStats

gmIsFinal :: Monadic Bool
gmIsFinal = do code <- getCode
               return (codeIsNull code)

dispatch :: Instruction -> Monadic () 
dispatch (Pushglobal f)  = doPushglobal f
dispatch (Pushint n)     = doPushint n
dispatch (Pushchar c)    = doPushchar c
dispatch (Push n)        = doPush n
dispatch (Mkap)          = doMkap
dispatch (Update n)      = doUpdate n
dispatch (Pop n)         = doPop n
dispatch (Unwind)        = doUnwind
dispatch (Slide n)       = doSlide n
dispatch (Alloc n)       = doAlloc n
dispatch (Eval)          = doEval
dispatch (IAdd)          = arithmetic2 (+)
dispatch (ISub)          = arithmetic2 (-)
dispatch (IMul)          = arithmetic2 (*)
dispatch (IDiv)          = arithmetic2 div
dispatch (INegate)       = arithmetic1 negate
dispatch (IEQT)          = comparison (==)
dispatch (INEQT)         = comparison (/=)
dispatch (ILT)           = comparison (<)
dispatch (ILE)           = comparison (<=)
dispatch (IGT)           = comparison (>)
dispatch (IGE)           = comparison (>=)
dispatch (COrd)          = charToInt ord
dispatch (CChr)          = intToChar chr
dispatch (Cond i1 i2)    = doCond i1 i2
dispatch (Pack t n)      = doPack t n
dispatch (Casejump alts) = doCasejump alts
dispatch (Split n)       = doSplit n
dispatch (Print)         = doPrint
dispatch (PrintStr str)  = doPrintStr str
dispatch (Mkbool)        = doMkbool
dispatch (Mkint)         = doMkint
dispatch (Mkchar)        = doMkchar
dispatch (Get)           = doGet
dispatch (Pushbasic n)   = doPushbasic n
dispatch (Return)        = doReturn




-------------------------------------------------------
-- Auxiliar functions 
-------------------------------------------------------

-- Heap

doHeapAlloc :: Node -> Monadic Addr
doHeapAlloc n = do heap <- getHeap
                   let (newHeap, a) = heapAlloc heap n
                   putHeap newHeap
                   stats <- getStats
                   let newStats = statIncHeaps stats
                   putStats newStats
                   return a

-- SStack

doSStackPush :: Addr -> Monadic ()
doSStackPush a = do sstack <- getSStack
                    let newSStack = sstackPush sstack a
                    putSStack newSStack
                    stats <- getStats
                    let newStats = statUpdateSStacks stats (sstackLength newSStack)
                    putStats newStats

doSStackSet :: GMSStack -> Monadic ()                   
doSStackSet newSStack = do putSStack newSStack
                           stats <- getStats
                           let newStats = statUpdateSStacks stats (sstackLength newSStack)
                           putStats newStats
                   
doSStackPop :: Monadic Addr
doSStackPop = do sstack <- getSStack
                 case sstackPop sstack of
                  Nothing           -> raiseError ("doSStackPop: empty sstack")
                  Just (sstack', a0) -> do doSStackSet sstack'
                                           return a0                                    

doSStackDrop :: Int -> Monadic ()
doSStackDrop n = do sequence_ (replicate n doSStackPop) `mplus` raiseError ("doSStackDrop: empty sstack")


doSStackTop :: Monadic Addr
doSStackTop = do sstack <- getSStack
                 case sstackPop sstack of
                  Nothing           -> raiseError ("doSStackTop: empty sstack")
                  Just (sstack', a0) -> return a0                                    

doSStackSelect :: Int -> Monadic Addr
doSStackSelect n = do sstack <- getSStack
                      case sstackSelect sstack n of
                       Nothing -> raiseError ("doSStackSelect: sstack too small")
                       Just an -> return an

-- VStack
              
doVStackPush :: BasicValue -> Monadic ()
doVStackPush n = do vstack <- getVStack
                    let newVStack = vstackPush vstack n
                    putVStack newVStack
                    stats <- getStats
                    let newStats = statUpdateVStacks stats (vstackLength newVStack)
                    putStats newStats

doVStackSet :: GMVStack -> Monadic ()                   
doVStackSet newVStack = do putVStack newVStack
                           stats <- getStats
                           let newStats = statUpdateVStacks stats (vstackLength newVStack)
                           putStats newStats

doVStackPop :: Monadic BasicValue
doVStackPop = do vstack <- getVStack
                 case vstackPop vstack of
                  Nothing            -> raiseError ("doVStackPop: empty sstack")
                  Just (vstack', n0) -> do doVStackSet vstack'
                                           return n0


-- Dump

doDumpSet :: GMDump -> Monadic ()
doDumpSet newDump = do putDump newDump
                       stats <-  getStats
                       let newStats = statUpdateDumps stats (dumpLength newDump)
                       putStats newStats
                       

-------------------------------------------------------

doPushglobal :: Name -> Monadic ()
doPushglobal f = do globals <- getGlobals
                    case globalLookup globals f of
                      Nothing -> raiseError ("doPushglobal: Undeclared global "`mplus`f)
                      Just a  -> doSStackPush a 

doPushint :: Int -> Monadic ()
doPushint n = do a <- doHeapAlloc (NInt n)
                 doSStackPush a

doPushchar :: Char -> Monadic ()
doPushchar c = do a <- doHeapAlloc (NChar c)
                  doSStackPush a

doPush :: Int -> Monadic ()
doPush n = do an <- doSStackSelect n `mplus` raiseError ("doPush: sstack too small")
              doSStackPush an
              

doMkap :: Monadic ()
doMkap = do a0 <- doSStackPop `mplus` raiseError ("doMkap: empty sstack")
            a1 <- doSStackPop `mplus` raiseError ("doMkap: empty sstack")
            a  <- doHeapAlloc (NAp a0 a1)
            doSStackPush a


doUpdate :: Int -> Monadic ()
doUpdate n = do a    <- doSStackPop `mplus` raiseError ("doUpdate: empty sstack")
                an   <- doSStackSelect n `mplus` raiseError ("doUpdate: sstack too small")
                heap <- getHeap
                let newHeap = heapUpdate heap an (NInd a)
                putHeap newHeap

               
doPop :: Int -> Monadic ()
doPop n = do doSStackDrop n `mplus` raiseError ("doPop: empty sstack")


doUnwind :: Monadic ()
doUnwind = do a <- doSStackTop `mplus` raiseError ("doUnwind: empty sstack")
              heap  <- getHeap
              case heapLookup heap a of
                Nothing   -> raiseError ("doUnwind: can't find address " ++show a)
                Just node -> newState node
           where newState (NInt _)      = do a <- doSStackTop
                                             dump <- getDump
                                             case dumpPop dump of
                                                    Nothing -> raiseError ("doUnwing: empty dump")
                                                    Just (d, i', s') -> do putCode i'
                                                                           let newSStack = sstackPush s' a             
                                                                           doSStackSet newSStack
                                                                           doDumpSet d
                 newState (NChar _)     = do a <- doSStackTop
                                             dump <- getDump
                                             case dumpPop dump of
                                                    Nothing -> raiseError ("doUnwing: empty dump")
                                                    Just (d, i', s') -> do putCode i'
                                                                           let newSStack = sstackPush s' a             
                                                                           doSStackSet newSStack
                                                                           doDumpSet d
                 newState (NConstr _ _) = do a <- doSStackTop
                                             dump <- getDump
                                             case dumpPop dump of
                                                    Nothing -> raiseError ("doUnwing: empty dump")
                                                    Just (d, i', s') -> do putCode i'
                                                                           let newSStack = sstackPush s' a             
                                                                           doSStackSet newSStack
                                                                           doDumpSet d
                                                                                            
                 newState (NAp a1 a2)   = do doSStackPush a1
                                             putCode (codeFromList [Unwind])
                 newState (NGlobal n c) = do sstack <- getSStack
                                             let k = sstackLength sstack - 1
                                             if k < n then do ak <- doSStackSelect k
                                                              dump <- getDump
                                                              case dumpPop dump of
                                                                     Nothing -> raiseError ("doUnwing: empty dump")
                                                                     Just (d, i, s) -> do putCode i
                                                                                          let newSStack = sstackPush s ak
                                                                                          doSStackSet newSStack
                                                                                          doDumpSet d
                                                      else do putCode c
                                                              rearrangeSStack n
                 newState (NInd a)      = do a0 <- doSStackPop `mplus` raiseError ("doUnwind: empty sstack")
                                             doSStackPush a
                                             putCode (codeFromList [Unwind])

     
                                                
rearrangeSStack :: Int -> Monadic ()
rearrangeSStack n = do sstack <- getSStack
                       let as = sstackToList sstack
                       as' <- mapM f (tail as)
                       let newSStack = sstackFromList (take n as' ++ drop n as)
                       doSStackSet newSStack
    where f a = do heap <- getHeap
                   case heapLookup heap a of
                    Nothing          -> raiseError ("rearrangeSStack: can`t find address " ++ showAddr a++" in heap")
                    Just (NAp a1 a2) -> return a2
                    Just node        -> raiseError ("rearrangeSStack: NAp node expected in sstack")
                          

doSlide :: Int -> Monadic ()
doSlide n = do a0 <- doSStackPop `mplus` raiseError ("doSlide: empty sstack")
               doSStackDrop n `mplus` raiseError ("doSlide: empty sstack")
               doSStackPush a0
               

doAlloc :: Int -> Monadic ()
doAlloc n = sequence_ (replicate n doAllocANode)
  where doAllocANode = do a <- doHeapAlloc (NInd heapNull)
                          doSStackPush a



doEval :: Monadic ()
doEval = do a <- doSStackPop `mplus` raiseError ("doEval: empty sstack")
            s <- getSStack
            d <- getDump
            i <- getCode
            let newSStack = sstackFromList [a]
            doSStackSet newSStack
            let newDump = dumpPush d (i,s)
            doDumpSet newDump
            let newCode = codeFromList [Unwind]
            putCode newCode
            
{-
doCond :: GMCode -> GMCode -> Monadic ()
doCond i1 i2 = do a <- doSStackPop `mplus` raiseError ("doCond: empty sstack")
                  heap <- getHeap
                  case heapLookup heap a of
                    Nothing       -> raiseError ("doCond: can`t find address "++showAddr a++" in heap")
                    Just (NInt 1) -> do code <- getCode
                                        putCode (i1 ++ code)
                    Just (NInt 0) -> do code <- getCode
                                        putCode (i2 ++ code)
                    Just _        -> raiseError ("doCond: non boolean node in sstack")
-}
{-
doCond :: GMCode -> GMCode -> Monadic ()
doCond i1 i2 = do a <- doSStackPop `mplus` raiseError ("doCond: empty sstack")
                  heap <- getHeap
                  case heapLookup heap a of
                    Nothing             -> raiseError ("doCond: can`t find address "++showAddr a++" in heap")
                    Just (NConstr 2 []) -> do code <- getCode
                                              putCode (i1 ++ code)
                    Just (NConstr 1 []) -> do code <- getCode
                                              putCode (i2 ++ code)
                    Just _              -> raiseError ("doCond: non boolean node in sstack")
-}

doCond :: GMCode -> GMCode -> Monadic ()
doCond i1 i2 = do BasicInt n <- doVStackPop `mplus` raiseError ("doCond: vstack is empty or integer not found")
                  if n==tagTrue then do code <- getCode       
                                        putCode (i1 `mplus` code)
                   else if n==tagFalse then do code <- getCode       
                                               putCode (i2 `mplus` code)
                   else raiseError ("doCond: non boolean value in vstack")


doPack :: Int -> Int -> Monadic ()
doPack t n = do as <- sequence (replicate n doSStackPop) `mplus` raiseError ("doPack: unsaturated constructor")
                a <- doHeapAlloc (NConstr t as)
                doSStackPush a
                

doCasejump :: [(Int,GMCode)] -> Monadic ()
doCasejump alts = do a <- doSStackTop `mplus` raiseError ("doCasejump: NConstr node expected on sstack")
                     heap <- getHeap
                     case heapLookup heap a of
                       Just (NConstr t ss) -> case [i | (tag,i)<-alts, tag==t] of
                                                []   -> raiseError ("doCasejump: no code for tag "++show t)
                                                i':_ -> do i <- getCode
                                                           putCode (i' `mplus` i)
                       Just _              -> raiseError ("doCasejump: NConstr node expected on sstack")
                       Nothing             -> raiseError ("doCasejump: can`t find address "++showAddr a++" in heap")


doSplit :: Int -> Monadic ()
doSplit n = do a <- doSStackPop `mplus` raiseError ("doSplit: empty sstack")
               heap <- getHeap
               case heapLookup heap a of
                 Just (NConstr t as) -> sequence_ (map doSStackPush (reverse as))
                 Just _              -> raiseError ("doSplit: NConstr node expected on sstack")
                 Nothing             -> raiseError ("doSplit: can`t find address "++showAddr a++" in heap")


doPrint :: Monadic ()
doPrint = do a <- doSStackPop `mplus` raiseError ("doPrint: empty sstack")
             heap <-  getHeap
             case heapLookup heap a of
               Just (NInt n)       -> putStr (show n)
               Just (NChar c)      -> putStr (show c)               
               Just (NConstr t []) -> do let name = if t==tagFalse then tkFalse
               					     else if t==tagTrue then tkTrue
               					     else if t==tagNil then tkNil
               					     else "C"++show t
               				 let iBegin = codeFromList [PrintStr name]
                                         i <- getCode
                                         putCode (iBegin `mplus` i)               
               Just (NConstr t as) -> do sequence (map doSStackPush (reverse as))
                                         i <- getCode
					 let name = if t==tagCons then tkCons++"("
					             else if t==tagTuple2 then "("
					             else if t==tagTuple3 then "("
               					     else "C"++show t++"("                                         
                                         let iBegin = codeFromList [PrintStr name]
                                         let i1' = codeFromList (concat (replicate (length as-1) [Eval,Print,PrintStr ","]))
                                         let i2' = codeFromList [Eval,Print]
                                         let iEnd = codeFromList [PrintStr ")"]                                         
                                         putCode (iBegin `mplus` i1' `mplus` i2' `mplus` iEnd `mplus` i)
               Just (NAp _ _)      -> do i <- getCode
                                         putCode (codeFromList [PrintStr "Aplication"]`mplus` i)                          
               Nothing             -> raiseError ("doPrint: NInt, NChar or NConstr expected on sstack")


doPrintStr :: String -> Monadic ()
doPrintStr str = putStr str

doMkbool :: Monadic ()
doMkbool = do BasicInt x <- doVStackPop `mplus` raiseError ("doMkbool: vstack is empty or integer not found")
              a <- doHeapAlloc (NConstr x [])
              doSStackPush a

doMkint :: Monadic ()
doMkint = do BasicInt x <- doVStackPop `mplus` raiseError ("doMkint: vstack is empty or integer not found")
             a <- doHeapAlloc (NInt x)
             doSStackPush a

doMkchar :: Monadic ()
doMkchar = do BasicChar x <- doVStackPop `mplus` raiseError ("doMkchar: vstack is empty or char not found")
              a <- doHeapAlloc (NChar x)
              doSStackPush a

doGet :: Monadic ()
doGet = do a <- doSStackPop `mplus` raiseError ("doGet: empty sstack")
           n <- value a
           doVStackPush n
        where value a = do heap <- getHeap
                           case heapLookup heap a of
                            Just (NInt n)              -> return (BasicInt n)
                            Just (NChar c)             -> return (BasicChar c)
                            Just (NConstr tag  [])     -> return (BasicInt tagTrue)
                            Just (NInd a')             -> value a'
                            _                          -> raiseError ("doGet: boolean or int expected in sstack")
                            
                            
doPushbasic :: BasicValue -> Monadic ()                            
doPushbasic n = doVStackPush n


doReturn :: Monadic ()
doReturn = raiseError "doReturn non implemented"

               


--------------------------------------------------------
-- Boxing and Unboxing
--------------------------------------------------------

boxInteger :: Int -> Monadic ()
boxInteger n = do a <- doHeapAlloc (NInt n)
                  doSStackPush a

boxBoolean :: Bool -> Monadic ()
boxBoolean b = do let n = if b then tagTrue
                               else tagFalse
                  a <- doHeapAlloc (NConstr n [])
                  doSStackPush a

unboxInteger :: Addr -> Monadic Int
unboxInteger a = do heap <- getHeap
                    case heapLookup heap a of
                      Nothing       -> raiseError ("unboxInteger: can't find address "++showAddr a++" in heap")
                      Just (NInt i) -> return i
                      Just _        -> raiseError ("unboxInteger: non-integer")

primitive1 :: (Int->Int) -> Monadic ()
primitive1 op = do BasicInt n <- doVStackPop `mplus` raiseError ("primitive1: empty vstack or integer not found")
                   doVStackPush (BasicInt (op n))

primitive2 :: (Int->Int->Int) -> Monadic ()
primitive2 op = do BasicInt n0 <- doVStackPop `mplus` raiseError ("primitive2: empty vstack or integer not found")
                   BasicInt n1 <- doVStackPop `mplus` raiseError ("primitive2: empty vstack or integer not found")
                   doVStackPush (BasicInt (op n0 n1))

arithmetic1 :: (Int->Int) -> Monadic ()
arithmetic1 = primitive1

arithmetic2 :: (Int->Int->Int) -> Monadic ()
arithmetic2 = primitive2

comparison :: (Int->Int->Bool) -> Monadic ()
comparison op = primitive2 op'
   where op' x y | op x y    = tagTrue
                 | otherwise = tagFalse


charToInt :: (Char->Int) -> Monadic ()
charToInt op = do BasicChar c <- doVStackPop `mplus` raiseError ("charToInt: empty vstack or char not found")
                  doVStackPush (BasicInt (op c))
                  
intToChar :: (Int->Char) -> Monadic ()
intToChar op = do BasicInt n <- doVStackPop `mplus` raiseError ("intToChar: empty vstack or int not found")
                  doVStackPush (BasicChar (op n))
                  

trace :: Monadic ()
trace = do showSCs
           eval'
       where eval' :: Monadic ()
             eval' = do showState
                        step
                        doAdmin
                        final <- gmIsFinal
                        if final then showStats
                                 else eval'

eval :: Monadic ()
eval = do step
          doAdmin
          final <- gmIsFinal
          if final then do showStats
                   else eval


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

