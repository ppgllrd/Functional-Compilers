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
import GMStack
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
                     let newST = MkGMState { gmCode    = gmCode',
                                             gmStack   = gmStack st,
                                             gmDump    = gmDump st,
                                             gmHeap    = gmHeap st,
                                             gmGlobals = gmGlobals st,
                                             gmStats   = gmStats st }
                     writeST newST

getStack :: Monadic GMStack
getStack = do st <- readST
              return (gmStack st)

putStack :: GMStack -> Monadic ()
putStack gmStack' = do st <- readST
                       let newST = MkGMState { gmCode    = gmCode st,
                                               gmStack   = gmStack',
                                               gmDump    = gmDump st,
                                               gmHeap    = gmHeap st,
                                               gmGlobals = gmGlobals st,
                                               gmStats   = gmStats st }
                       writeST newST

getHeap :: Monadic GMHeap
getHeap = do st <- readST
             return (gmHeap st)

putHeap :: GMHeap -> Monadic ()
putHeap gmHeap' = do st <- readST
                     let newST = MkGMState { gmCode    = gmCode st,
                                             gmStack   = gmStack st,
                                             gmDump    = gmDump st,
                                             gmHeap    = gmHeap',
                                             gmGlobals = gmGlobals st,
                                             gmStats   = gmStats st }
                     writeST newST

getGlobals :: Monadic GMGlobals
getGlobals = do st <- readST
                return (gmGlobals st)

putGlobals :: GMGlobals -> Monadic ()
putGlobals gmGlobals' = do st <- readST
                           let newST = MkGMState { gmCode    = gmCode st,
                                                   gmStack   = gmStack st,
                                                   gmDump    = gmDump st,
                                                   gmHeap    = gmHeap st,
                                                   gmGlobals = gmGlobals',
                                                   gmStats   = gmStats st }
                           writeST newST
 
getStats :: Monadic GMStats
getStats = do st <- readST
              return (gmStats st)

putStats :: GMStats -> Monadic ()
putStats gmStats' = do st <- readST
                       let newST = MkGMState { gmCode    = gmCode st,
                                               gmStack   = gmStack st,
                                               gmDump    = gmDump st,
                                               gmHeap    = gmHeap st,
                                               gmGlobals = gmGlobals st,
                                               gmStats   = gmStats' }
                       writeST newST

getDump :: Monadic GMDump
getDump = do st <- readST
             return (gmDump st)

putDump :: GMDump -> Monadic ()
putDump gmDump' = do st <- readST
                     let newST = MkGMState { gmCode    = gmCode st,
                                             gmStack   = gmStack st,
                                             gmDump    = gmDump',
                                             gmHeap    = gmHeap st,
                                             gmGlobals = gmGlobals st,
                                             gmStats   = gmStats st }
                     writeST newST


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
dispatch (Pushglobal f) = doPushglobal f
dispatch (Pushint n)    = doPushint n
dispatch (Push n)       = doPush n
dispatch (Mkap)         = doMkap
dispatch (Update n)     = doUpdate n
dispatch (Pop n)        = doPop n
dispatch (Unwind)       = doUnwind
dispatch (Slide n)      = doSlide n
dispatch (Alloc n)      = doAlloc n
dispatch (Eval)         = doEval
dispatch (IAdd)         = arithmetic2 (+)
dispatch (ISub)         = arithmetic2 (-)
dispatch (IMul)         = arithmetic2 (*)
dispatch (IDiv)         = arithmetic2 div
dispatch (INegate)      = arithmetic1 negate
dispatch (IEQT)         = comparison (==)
dispatch (INEQT)        = comparison (/=)
dispatch (ILT)          = comparison (<)
dispatch (ILE)          = comparison (<=)
dispatch (IGT)          = comparison (>)
dispatch (IGE)          = comparison (>=)
dispatch (Cond i1 i2)   = doCond i1 i2



-------------------------------------------------------
-- Auxiliar functions 
-------------------------------------------------------

doHeapAlloc :: Node -> Monadic Addr
doHeapAlloc n = do heap <- getHeap
                   let (newHeap, a) = heapAlloc heap n
                   putHeap newHeap
                   stats <- getStats
                   let newStats = statIncHeaps stats
                   putStats newStats
                   return a

doStackPush :: Addr -> Monadic ()
doStackPush a = do stack <- getStack
                   let newStack = stackPush stack a
                   putStack newStack
                   stats <- getStats
                   let newStats = statUpdateStacks stats (stackLength newStack)
                   putStats newStats

doStackSet :: GMStack -> Monadic ()                   
doStackSet newStack = do putStack newStack
                         stats <- getStats
                         let newStats = statUpdateStacks stats (stackLength newStack)
                         putStats newStats
                   
doStackPop :: Monadic Addr
doStackPop = do stack <- getStack
                case stackPop stack of
                  Nothing           -> raiseError ("doStackPop: empty stack")
                  Just (stack', a0) -> do doStackSet stack'
                                          return a0                                    

doStackDrop :: Int -> Monadic ()
doStackDrop n = do sequence (replicate n doStackPop) ++ raiseError ("doStackDrop: empty stack")


doStackTop :: Monadic Addr
doStackTop = do stack <- getStack
                case stackPop stack of
                  Nothing           -> raiseError ("doStackTop: empty stack")
                  Just (stack', a0) -> return a0                                    

doStackSelect :: Int -> Monadic Addr
doStackSelect n = do stack <- getStack
                     case stackSelect stack n of
                       Nothing -> raiseError ("doStackSelect: stack too small")
                       Just an -> return an
              


{-
stack  <- getStack
                   stack' <- sDrop stack n
                   putStack stack'
                where sDrop stack 0     = return stack
                      sDrop stack (n+1) = case stackPop stack of
                                            Nothing          -> raiseError ("doStackDrop: empty stack")
                                            Just (stack', _) -> sDrop stack' n
-}                   
-------------------------------------------------------

doPushglobal :: Name -> Monadic ()
doPushglobal f = do globals <- getGlobals
                    case globalLookup globals f of
                      Nothing -> raiseError ("doPushglobal: Undeclared global "++f)
                      Just a  -> doStackPush a 

doPushint :: Int -> Monadic ()
doPushint n = do a <- doHeapAlloc (NNum n)
                 doStackPush a


doPush :: Int -> Monadic ()
doPush n = do an <- doStackSelect n ++ raiseError ("doPush: stack too small")
              doStackPush an
              

doMkap :: Monadic ()
doMkap = do a0 <- doStackPop ++ raiseError ("doMkap: empty stack")
            a1 <- doStackPop ++ raiseError ("doMkap: empty stack")
            a  <- doHeapAlloc (NAp a0 a1)
            doStackPush a


doUpdate :: Int -> Monadic ()
doUpdate n = do a    <- doStackPop ++ raiseError ("doUpdate: empty stack")
                an   <- doStackSelect n ++ raiseError ("doUpdate: stack too small")
                heap <- getHeap
                let newHeap = heapUpdate heap an (NInd a)
                putHeap newHeap

               
doPop :: Int -> Monadic ()
doPop n = do doStackDrop n ++ raiseError ("doPop: empty stack")


doUnwind :: Monadic ()
doUnwind = do a <- doStackTop ++ raiseError ("doUnwind: empty stack")
              heap  <- getHeap
              case heapLookup heap a of
                Nothing   -> raiseError ("doUnwind: can't find address " ++show a)
                Just node -> newState node
           where newState (NNum _)      = do a <- doStackTop
                                             dump <- getDump
                                             case dumpPop dump of
                                                    Nothing -> raiseError ("doUnwing: empty dump")
                                                    Just (d, i', s') -> do putCode i'
                                                                           let newStack = stackPush s' a             
                                                                           doStackSet newStack
                                                                           putDump d
                 newState (NAp a1 a2)   = do doStackPush a1
                                             putCode (codeFromList [Unwind])
                 newState (NGlobal n c) = do stack <- getStack
                                             let k = stackLength stack - 1
                                             if k < n then do raiseError ("doUnwing: too few arguments")
                                                      else do putCode c
                                                              rearrangeStack n
                 newState (NInd a)      = do a0 <- doStackPop ++ raiseError ("doUnwind: empty stack")
                                             doStackPush a
                                             putCode (codeFromList [Unwind])

     
                                                
rearrangeStack :: Int -> Monadic ()
rearrangeStack n = do stack <- getStack
                      let as = stackToList stack
                      as' <- mapM f (tail as)
                      let newStack = stackFromList (take n as' ++ drop n as)
                      doStackSet newStack
    where f a = do heap <- getHeap
                   case heapLookup heap a of
                    Nothing          -> raiseError ("rearrangeStack: can`t find address "++showAddr a++" in heap")
                    Just (NAp a1 a2) -> return a2
                    Just node        -> raiseError ("rearrangeStack: NAp node expected in stack")
                          

doSlide :: Int -> Monadic ()
doSlide n = do a0 <- doStackPop ++ raiseError ("doSlide: empty stack")
               doStackDrop n ++ raiseError ("doSlide: empty stack")
               doStackPush a0
               

doAlloc :: Int -> Monadic ()
doAlloc n = sequence (replicate n doAllocANode)
  where doAllocANode =do a <- doHeapAlloc (NInd heapNull)
                         doStackPush a



doEval :: Monadic ()
doEval = do a <- doStackPop ++ raiseError ("doEval: empty stack")
            s <- getStack
            d <- getDump
            i <- getCode
            let newStack = stackFromList [a]
            doStackSet newStack
            let newDump = dumpPush d (i,s)
            putDump newDump
            let newCode = codeFromList [Unwind]
            putCode newCode
            

doCond :: GMCode -> GMCode -> Monadic ()
doCond i1 i2 = do a <- doStackPop ++ raiseError ("doCond: empty stack")
                  heap <- getHeap
                  case heapLookup heap a of
                    Nothing       -> raiseError ("doCond: can`t find address "++showAddr a++" in heap")
                    Just (NNum 1) -> do code <- getCode
                                        putCode (i1 ++ code)
                    Just (NNum 0) -> do code <- getCode
                                        putCode (i2 ++ code)
                    Just _        -> raiseError ("doCond: non boolean node in stack")

--------------------------------------------------------
-- Boxing and Unboxing
--------------------------------------------------------

boxInteger :: Int -> Monadic ()
boxInteger n = do a <- doHeapAlloc (NNum n)
                  doStackPush a

boxBoolean :: Bool -> Monadic ()
boxBoolean b = do let n = if b then 1 else 0
                  a <- doHeapAlloc (NNum n)
                  doStackPush a

unboxInteger :: Addr -> Monadic Int
unboxInteger a = do heap <- getHeap
                    case heapLookup heap a of
                      Nothing       -> raiseError ("unboxInteger: can't find address "++showAddr a++" in heap")
                      Just (NNum i) -> return i
                      Just _        -> raiseError ("unboxInteger: non-integer")

primitive1 :: (b -> Monadic ()) -> (Addr -> Monadic a) -> (a->b) -> Monadic ()
primitive1 box unbox op = do a <- doStackPop ++ raiseError ("primitive1: empty stack")
                             i <- unbox a
                             box (op i)

primitive2 :: (b -> Monadic ()) -> (Addr -> Monadic a) -> (a->a->b) -> Monadic ()
primitive2 box unbox op = do a0 <- doStackPop ++ raiseError ("primitive2: empty stack")
                             a1 <- doStackPop ++ raiseError ("primitive2: empty stack")
                             i0 <- unbox a0
                             i1 <- unbox a1
                             box (op i0 i1)



arithmetic1 :: (Int->Int) -> Monadic ()
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int->Int->Int) -> Monadic ()
arithmetic2 = primitive2 boxInteger unboxInteger

comparison :: (Int->Int->Bool) -> Monadic ()
comparison = primitive2 boxBoolean unboxInteger


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
          if final then do showState
                           showStats
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
       iStr "  Code:{",
       iIndent (iInterleave iNewline (map toIseq is)),
       iStr "}", iNewline
     ]
   where is = codeToList code


iState :: GMState -> Iseq
iState s
   = iConcat [ 
       iStack s,                 iNewline,
       iDump s,                  iNewline,
       iInstructions (gmCode s), iNewline 
     ]

iStack :: GMState -> Iseq
iStack s
   = iConcat [
       iStr " Stack:[",
       iIndent (iInterleave iNewline
                       (map (iStackItem s) ((reverse.stackToList.gmStack) s))),
       iStr "]"
     ]

iStackItem :: GMState -> Addr -> Iseq
iStackItem s a
   = iConcat [
       iStr (showAddr a), iStr ": ",
       iNode s a node
     ]
     where Just node = heapLookup (gmHeap s) a

iNode :: GMState -> Addr -> Node -> Iseq
iNode s a (NNum n)      = iNum n
iNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
                              where Just v = globalLookupFromAddr (gmGlobals s) a

iNode s a (NAp a1 a2)   = iConcat [
                            iStr "Ap ", iStr (showAddr a1),
                            iStr " ",   iStr (showAddr a2)
                          ]
iNode s a (NInd a1)     = iConcat [
                            iStr "NInd ", iStr (showAddr a1)
                          ]

iStats :: GMState -> Iseq
iStats s
   = iConcat [
       iStr "Steps taken = ", iNum (statGetSteps (gmStats s)), iNewline,
       iStr "Heap nodes allocated = ", iNum (statGetHeaps (gmStats s)),iNewline,
       iStr "Maximum stack length = ", iNum (statGetStacks (gmStats s)),
       iNewline
     ]


iDump :: GMState -> Iseq
iDump s
   = iConcat [ iStr "  Dump:[",
               iIndent (iInterleave iNewline
                        (map iDumpItem (reverse (dumpToList (gmDump s))))),
               iStr "]"
     ]

iDumpItem :: (GMCode, GMStack) -> Iseq
iDumpItem (code, stack)
    = iConcat [ iStr "<",
                iShortCode 3 code, iStr ", ",
                iShortStack stack,
                iStr ">"
      ]

iShortStack :: GMStack -> Iseq
iShortStack stack 
     = iConcat [ iStr "[",
                 iInterleave (iStr ", ") (map (iStr . showAddr) (stackToList stack)),
                 iStr "]"
       ]

