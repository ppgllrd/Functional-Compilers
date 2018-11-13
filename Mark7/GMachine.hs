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
import LibSIO

import LibPretty
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
dispatch (Cond i1 i2)    = doCond i1 i2
dispatch (Pack t n)      = doPack t n
dispatch (Casejump alts) = doCasejump alts
dispatch (Split n)       = doSplit n
dispatch (Print)         = doPrint
dispatch (PrintStr str)  = doPrintStr str
dispatch (Mkbool)        = doMkbool
dispatch (Mkint)         = doMkint
dispatch (Get)           = doGet
dispatch (Pushbasic n)   = doPushbasic n
dispatch (Return)        = doReturn




-------------------------------------------------------
-- Auxiliar functions 
-------------------------------------------------------

-- Heap

doHeapAlloc :: Node -> Monadic Address
doHeapAlloc n = do heap <- getHeap
                   let (newHeap, a) = heapAlloc heap n
                   putHeap newHeap
                   stats <- getStats
                   let newStats = statIncHeaps stats
                   putStats newStats
                   return a

-- SStack

doSStackPush :: Address -> Monadic ()
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
                   
doSStackPop :: Monadic Address
doSStackPop = do sstack <- getSStack
                 case sstackPop sstack of
                  Nothing           -> raiseError ("doSStackPop: empty sstack")
                  Just (sstack', a0) -> do doSStackSet sstack'
                                           return a0                                    

doSStackDrop :: Int -> Monadic ()
doSStackDrop n = do sequence (replicate n doSStackPop) ++ raiseError ("doSStackDrop: empty sstack")


doSStackTop :: Monadic Address
doSStackTop = do sstack <- getSStack
                 case sstackPop sstack of
                  Nothing           -> raiseError ("doSStackTop: empty sstack")
                  Just (sstack', a0) -> return a0                                    

doSStackSelect :: Int -> Monadic Address
doSStackSelect n = do sstack <- getSStack
                      case sstackSelect sstack n of
                       Nothing -> raiseError ("doSStackSelect: sstack too small")
                       Just an -> return an

-- VStack
              
doVStackPush :: Int -> Monadic ()
doVStackPush n = do vstack <- getVStack
                    let newVStack = vstackPush vstack n
                    putVStack newVStack

doVStackSet :: GMVStack -> Monadic ()                   
doVStackSet newVStack = do putVStack newVStack

doVStackPop :: Monadic Int
doVStackPop = do vstack <- getVStack
                 case vstackPop vstack of
                  Nothing            -> raiseError ("doVStackPop: empty sstack")
                  Just (vstack', n0) -> do doVStackSet vstack'
                                           return n0


-------------------------------------------------------

doPushglobal :: Name -> Monadic ()
doPushglobal f = do globals <- getGlobals
                    case globalLookup globals f of
                      Nothing -> raiseError ("doPushglobal: Undeclared global "++f)
                      Just a  -> doSStackPush a 

doPushint :: Int -> Monadic ()
doPushint n = do a <- doHeapAlloc (NNum n)
                 doSStackPush a


doPush :: Int -> Monadic ()
doPush n = do an <- doSStackSelect n ++ raiseError ("doPush: sstack too small")
              doSStackPush an
              

doMkap :: Monadic ()
doMkap = do a0 <- doSStackPop ++ raiseError ("doMkap: empty sstack")
            a1 <- doSStackPop ++ raiseError ("doMkap: empty sstack")
            a  <- doHeapAlloc (NAp a0 a1)
            doSStackPush a


doUpdate :: Int -> Monadic ()
doUpdate n = do a    <- doSStackPop ++ raiseError ("doUpdate: empty sstack")
                an   <- doSStackSelect n ++ raiseError ("doUpdate: sstack too small")
                heap <- getHeap
                let newHeap = heapUpdate heap an (NInd a)
                putHeap newHeap

               
doPop :: Int -> Monadic ()
doPop n = do doSStackDrop n ++ raiseError ("doPop: empty sstack")


doUnwind :: Monadic ()
doUnwind = do a <- doSStackTop ++ raiseError ("doUnwind: empty sstack")
              heap  <- getHeap
              case heapLookup heap a of
                Nothing   -> raiseError ("doUnwind: can't find Addressess " ++show a)
                Just node -> newState node
           where newState (NNum _)      = do a <- doSStackTop
                                             dump <- getDump
                                             case dumpPop dump of
                                                    Nothing -> raiseError ("doUnwing: empty dump")
                                                    Just (d, i', s') -> do putCode i'
                                                                           let newSStack = sstackPush s' a             
                                                                           doSStackSet newSStack
                                                                           putDump d
                 newState (NConstr _ _) = do a <- doSStackTop
                                             dump <- getDump
                                             case dumpPop dump of
                                                    Nothing -> raiseError ("doUnwing: empty dump")
                                                    Just (d, i', s') -> do putCode i'
                                                                           let newSStack = sstackPush s' a             
                                                                           doSStackSet newSStack
                                                                           putDump d
                                                                                            
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
                                                                                          putDump d
                                                      else do putCode c
                                                              rearrangeSStack n
                 newState (NInd a)      = do a0 <- doSStackPop ++ raiseError ("doUnwind: empty sstack")
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
                    Nothing          -> raiseError ("rearrangeSStack: can`t find Addressess "++showAddress a++" in heap")
                    Just (NAp a1 a2) -> return a2
                    Just node        -> raiseError ("rearrangeSStack: NAp node expected in sstack")
                          

doSlide :: Int -> Monadic ()
doSlide n = do a0 <- doSStackPop ++ raiseError ("doSlide: empty sstack")
               doSStackDrop n ++ raiseError ("doSlide: empty sstack")
               doSStackPush a0
               

doAlloc :: Int -> Monadic ()
doAlloc n = sequence (replicate n doAllocANode)
  where doAllocANode = do a <- doHeapAlloc (NInd heapNull)
                          doSStackPush a



doEval :: Monadic ()
doEval = do a <- doSStackPop ++ raiseError ("doEval: empty sstack")
            s <- getSStack
            d <- getDump
            i <- getCode
            let newSStack = sstackFromList [a]
            doSStackSet newSStack
            let newDump = dumpPush d (i,s)
            putDump newDump
            let newCode = codeFromList [Unwind]
            putCode newCode
            
{-
doCond :: GMCode -> GMCode -> Monadic ()
doCond i1 i2 = do a <- doSStackPop ++ raiseError ("doCond: empty sstack")
                  heap <- getHeap
                  case heapLookup heap a of
                    Nothing       -> raiseError ("doCond: can`t find Addressess "++showAddress a++" in heap")
                    Just (NNum 1) -> do code <- getCode
                                        putCode (i1 ++ code)
                    Just (NNum 0) -> do code <- getCode
                                        putCode (i2 ++ code)
                    Just _        -> raiseError ("doCond: non boolean node in sstack")
-}
{-
doCond :: GMCode -> GMCode -> Monadic ()
doCond i1 i2 = do a <- doSStackPop ++ raiseError ("doCond: empty sstack")
                  heap <- getHeap
                  case heapLookup heap a of
                    Nothing             -> raiseError ("doCond: can`t find Addressess "++showAddress a++" in heap")
                    Just (NConstr 2 []) -> do code <- getCode
                                              putCode (i1 ++ code)
                    Just (NConstr 1 []) -> do code <- getCode
                                              putCode (i2 ++ code)
                    Just _              -> raiseError ("doCond: non boolean node in sstack")
-}

doCond :: GMCode -> GMCode -> Monadic ()
doCond i1 i2 = do n <- doVStackPop ++ raiseError ("doCond: empty vstack")
                  case n of
                    2 -> do code <- getCode       -- 2 is tag of True
                            putCode (i1 ++ code)
                    1 -> do code <- getCode       -- 1 is tag of True
                            putCode (i2 ++ code)
                    _ -> raiseError ("doCond: non boolean value in vstack")


doPack :: Int -> Int -> Monadic ()
doPack t n = do as <- accumulate (replicate n doSStackPop) ++ raiseError ("doPack: unsaturated constructor")
                a <- doHeapAlloc (NConstr t as)
                doSStackPush a
                

doCasejump :: [(Int,GMCode)] -> Monadic ()
doCasejump alts = do a <- doSStackTop ++ raiseError ("doCasejump: NConstr node expected on sstack")
                     heap <- getHeap
                     case heapLookup heap a of
                       Just (NConstr t ss) -> case [i | (tag,i)<-alts, tag==t] of
                                                []   -> raiseError ("doCasejump: no code for tag "++show t)
                                                i':_ -> do i <- getCode
                                                           putCode (i' ++ i)
                       Just _              -> raiseError ("doCasejump: NConstr node expected on sstack")
                       Nothing             -> raiseError ("doCasejump: can`t find Addressess "++showAddress a++" in heap")


doSplit :: Int -> Monadic ()
doSplit n = do a <- doSStackPop ++ raiseError ("doSplit: empty sstack")
               heap <- getHeap
               case heapLookup heap a of
                 Just (NConstr t as) -> sequence (map doSStackPush (reverse as))
                 Just _              -> raiseError ("doSplit: NConstr node expected on sstack")
                 Nothing             -> raiseError ("doSplit: can`t find Addressess "++showAddress a++" in heap")


doPrint :: Monadic ()
doPrint = do a <- doSStackPop ++ raiseError ("doPrint: empty sstack")
             heap <-  getHeap
             case heapLookup heap a of
               Just (NNum n)       -> putStr (show n)
               Just (NConstr t []) -> do let iBegin = codeFromList [PrintStr ("C"++show t)]
                                         i <- getCode
                                         putCode (iBegin ++ i)               
               Just (NConstr t as) -> do sequence (map doSStackPush (reverse as))
                                         i <- getCode
                                         let iBegin = codeFromList [PrintStr ("C"++show t++"(")]
                                         let i1' = codeFromList (concat (replicate (length as-1) [Eval,Print,PrintStr ","]))
                                         let i2' = codeFromList [Eval,Print]
                                         let iEnd = codeFromList [PrintStr ")"]                                         
                                         putCode (iBegin ++ i1' ++ i2' ++ iEnd ++ i)
               Nothing             -> raiseError ("doPrint: NNum or NConstr expected on sstack")


doPrintStr :: String -> Monadic ()
doPrintStr str = putStr str

doMkbool :: Monadic ()
doMkbool = do x <- doVStackPop ++ raiseError ("doMkbool: empty vstack")
              a <- doHeapAlloc (NConstr x [])
              doSStackPush a

doMkint :: Monadic ()
doMkint = do x <- doVStackPop ++ raiseError ("doMkint: empty vstack")
             a <- doHeapAlloc (NNum x)
             doSStackPush a


doGet :: Monadic ()
doGet = do a <- doSStackPop ++ raiseError ("doGet: empty sstack")
           n <- value a
           doVStackPush n
        where value a = do heap <- getHeap
                           case heapLookup heap a of
                            Just (NNum n)       -> return n
                            Just (NConstr 2 []) -> return 2  -- 2 is tag of True
                            Just (NConstr 1 []) -> return 1  -- 1 is tag of True
                            Just (NInd a')      -> value a'
                            _                   -> raiseError ("doGet: boolean or int expected in sstack")
                            
                            
doPushbasic :: Int -> Monadic ()                            
doPushbasic n = doVStackPush n


doReturn :: Monadic ()
doReturn = raiseError "doReturn non implemented"

               


--------------------------------------------------------
-- Boxing and Unboxing
--------------------------------------------------------

boxInteger :: Int -> Monadic ()
boxInteger n = do a <- doHeapAlloc (NNum n)
                  doSStackPush a

boxBoolean :: Bool -> Monadic ()
boxBoolean b = do let n = if b then 2 -- 2 is tag of True
                               else 1 -- 1 is tag of False
                  a <- doHeapAlloc (NConstr n [])
                  doSStackPush a

unboxInteger :: Address -> Monadic Int
unboxInteger a = do heap <- getHeap
                    case heapLookup heap a of
                      Nothing       -> raiseError ("unboxInteger: can't find Addressess "++showAddress a++" in heap")
                      Just (NNum i) -> return i
                      Just _        -> raiseError ("unboxInteger: non-integer")

primitive1 :: (Int->Int) -> Monadic ()
primitive1 op = do n <- doVStackPop ++ raiseError ("primitive1: empty vstack")
                   doVStackPush (op n)

primitive2 :: (Int->Int->Int) -> Monadic ()
primitive2 op = do n0 <- doVStackPop ++ raiseError ("primitive2: empty vstack")
                   n1 <- doVStackPop ++ raiseError ("primitive2: empty vstack")
                   doVStackPush (op n0 n1)

arithmetic1 :: (Int->Int) -> Monadic ()
arithmetic1 = primitive1

arithmetic2 :: (Int->Int->Int) -> Monadic ()
arithmetic2 = primitive2

comparison :: (Int->Int->Bool) -> Monadic ()
comparison op = primitive2 op'
   where op' x y | op x y    = 2 -- 2 is the tag of True
                 | otherwise = 1 -- 1 is the tag of False


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



iSC :: GMState -> (Name, Address) -> Iseq
iSC s (name, address)
   = iConcat [ 
       iStr "Code for ", iStr name, iNewline,
       iInstructions code, iNewline, iNewline
     ]
     where Just (NGlobal arity code) = heapLookup (gmHeap s) address

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

iSStackItem :: GMState -> Address -> Iseq
iSStackItem s a
   = iConcat [
       iStr (showAddress a), iStr ": ",
       iNode s a node
     ]
     where Just node = heapLookup (gmHeap s) a

iVStack :: GMState -> Iseq
iVStack s
   = iConcat [iStr " VStack:[",
              iInterleave (iStr ", ") (map iNum ((reverse.vstackToList.gmVStack) s)),
              iStr "]"
     ]         
   

iNode :: GMState -> Address -> Node -> Iseq
iNode s a (NNum n)       = iNum n
iNode s a (NGlobal n g)  = iConcat [iStr "Global ", iStr v]
                              where Just v = globalLookupFromAddress (gmGlobals s) a

iNode s a (NAp a1 a2)    = iConcat [
                             iStr "Ap ", iStr (showAddress a1),
                             iStr " ",   iStr (showAddress a2)
                           ]
iNode s a (NInd a1)      = iConcat [
                             iStr "Ind ", iStr (showAddress a1)
                           ]
iNode s a (NConstr t as) = iConcat [
                            iStr "Constr ", iNum t, iStr " [",
                            iInterleave (iStr ", ") (map (iStr.showAddress) as), iStr "]"
                          ]

iStats :: GMState -> Iseq
iStats s
   = iConcat [
       iNewline,
       iStr "Steps taken = ", iNum (statGetSteps (gmStats s)), iNewline,
       iStr "Heap nodes allocated = ", iNum (statGetHeaps (gmStats s)),iNewline,
       iStr "Maximum sstack length = ", iNum (statGetSStacks (gmStats s)),
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
                 iInterleave (iStr ", ") (map (iStr . showAddress) (sstackToList sstack)),
                 iStr "]"
       ]

