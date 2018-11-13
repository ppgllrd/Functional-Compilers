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



doPushglobal :: Name -> Monadic ()
doPushglobal f = do globals <- getGlobals
                    case globalLookup globals f of
                      Nothing -> raiseError ("doPushglobal: Undeclared global "++f)
                      Just a  -> do stack <- getStack
                                    let newStack = stackPush stack a
                                    putStack newStack
                                  

doPushint :: Int -> Monadic ()
doPushint n = do heap <- getHeap
                 let (newHeap, a) = heapAlloc heap (NNum n)
                 putHeap newHeap
                 stack <- getStack
                 let newStack = stackPush stack a
                 putStack newStack


doPush :: Int -> Monadic ()
doPush n = do stack <- getStack
              case stackSelect stack n of
                Nothing -> raiseError ("doPush: stack too small")
                Just an -> do let newStack = stackPush stack an
                              putStack newStack
              

doMkap :: Monadic ()
doMkap = do stack <- getStack
            case stackPop stack of
              Nothing           -> raiseError ("doMkap: empty stack")
              Just (stack', a0) -> case stackPop stack' of
                                     Nothing            -> raiseError ("doMkap: empty stack")
                                     Just (stack'', a1) -> do heap <- getHeap
                                                              let (newHeap, a) = heapAlloc heap (NAp a0 a1)
                                                              let newStack = stackPush stack'' a
                                                              putStack newStack
                                                              putHeap newHeap


doUpdate :: Int -> Monadic ()
doUpdate n = do stack <- getStack
                case stackPop stack of
                 Nothing          -> raiseError ("doUpdate: empty stack")
                 Just (stack', a) -> do case stackSelect stack' n of
                                          Nothing -> raiseError ("doUpdate: stack too small")
                                          Just an -> do heap <- getHeap
                                                        let newHeap = heapUpdate heap an (NInd a)
                                                        putHeap newHeap
                                                        putStack stack'

doPop :: Int -> Monadic ()
doPop n = do stack <- getStack
             stack' <- sDrop stack n
             putStack stack'
                where sDrop stack 0     = return stack
                      sDrop stack (n+1) = case stackPop stack of
                                            Nothing          -> raiseError ("doSlide: empty stack")
                                            Just (stack', _) -> sDrop stack' n



doUnwind :: Monadic ()
doUnwind = do stack <- getStack
              heap  <- getHeap
              case stackPop stack of
                 Nothing          -> raiseError ("doUnwind: empty stack")
                 Just (stack', a) -> case heapLookup heap a of
                                       Nothing   -> raiseError ("doUnwind: can't find address " ++show a)
                                       Just node -> newState node
                where newState (NNum _)      = do stack <- getStack
                                                  let Just (s, a) = stackPop stack
                                                  dump <- getDump
                                                  case dumpPop dump of
                                                    Nothing -> raiseError ("doUnwing: empty dump")
                                                    Just (d, i', s') -> do putCode i'
                                                                           let newStack = stackPush s' a             
                                                                           putStack newStack
                                                                           putDump d
                      newState (NAp a1 a2)   = do stack <- getStack
                                                  let newStack = stackPush stack a1
                                                  putStack newStack
                                                  putCode (codeFromList [Unwind])
                      newState (NGlobal n c) = do stack <- getStack
                                                  if stackLength stack <= n then raiseError "Unwinding with too few arguments. Type error"
                                                                            else do heap <- getHeap
                                                                                    putCode c
                                                                                    rearrangeStack n
                      newState (NInd a)      = do stack <- getStack
                                                  let Just (stack', a0) = stackPop stack
                                                  let stack'' = stackPush stack' a
                                                  putStack stack'' 
                                                  code <- getCode
                                                  if codeIsNull code then putCode (codeFromList [Unwind])
                                                                     else raiseError ("doUnwind: code should be empty")
     
                                                
rearrangeStack :: Int -> Monadic ()
rearrangeStack n = do stack <- getStack
                      let as = stackToList stack
                      as' <- mapM f (tail as)
                      let newStack = stackFromList (take n as' ++ drop n as)
                      putStack newStack
    where f a = do heap <- getHeap
                   case heapLookup heap a of
                    Nothing          -> raiseError ("rearrangeStack: can`t find address "++showAddr a++" in heap")
                    Just (NAp a1 a2) -> return a2
                    Just node        -> raiseError ("rearrangeStack: NAp node expected in stack")
                          

doSlide :: Int -> Monadic ()
doSlide n = do stack <- getStack
               case stackPop stack of
                 Nothing           -> raiseError ("doSlide: empty stack")
                 Just (stack', a0) -> do stack'' <- sDrop stack' n
                                         let newStack = stackPush stack'' a0
                                         putStack newStack
                where sDrop stack 0     = return stack
                      sDrop stack (n+1) = case stackPop stack of
                                            Nothing          -> raiseError ("doSlide: empty stack")
                                            Just (stack', _) -> sDrop stack' n



doAlloc :: Int -> Monadic ()
doAlloc 0     = return ()
doAlloc (n+1) = do stack <- getStack
                   heap <- getHeap
                   let (newHeap, a) = heapAlloc heap (NInd heapNull)
                   let newStack = stackPush stack a
                   putHeap newHeap
                   putStack newStack
                   doAlloc n


doEval :: Monadic ()
doEval = do stack <- getStack
            case stackPop stack of
              Nothing     -> raiseError ("doEval: empty stack")
              Just (s, a) -> do let newStack = stackFromList [a]
                                putStack newStack
                                d <- getDump
                                i <- getCode
                                let newDump = dumpPush d (i,s)
                                putDump newDump
                                let newCode = codeFromList [Unwind]
                                putCode newCode


doCond :: GMCode -> GMCode -> Monadic ()
doCond i1 i2 = do stack <- getStack
                  case stackPop stack of
                    Nothing    -> raiseError ("doCond: empty stack")
                    Just (s,a) -> do heap <- getHeap
                                     case heapLookup heap a of
                                       Nothing       -> raiseError ("doCond: can`t find address "++showAddr a++" in heap")
                                       Just (NNum 1) -> do code <- getCode
                                                           putStack s
                                                           putCode (i1 ++ code)
                                       Just (NNum 0) -> do code <- getCode
                                                           putStack s
                                                           putCode (i2 ++ code)
                                       Just _        -> raiseError ("doCond: non boolean node in stack")

--------------------------------------------------------
-- Boxing and Unboxing
--------------------------------------------------------

boxInteger :: Int -> Monadic ()
boxInteger n = do heap <- getHeap
                  let (h',a) = heapAlloc heap (NNum n)
                  stack <- getStack
                  let newStack = stackPush stack a
                  putHeap h'
                  putStack newStack

boxBoolean :: Bool -> Monadic ()
boxBoolean b = do heap <- getHeap
                  let n = if b then 1 else 0
                  let (h',a) = heapAlloc heap (NNum n)
                  stack <- getStack
                  let newStack = stackPush stack a
                  putHeap h'
                  putStack newStack

unboxInteger :: Addr -> Monadic Int
unboxInteger a = do heap <- getHeap
                    case heapLookup heap a of
                      Nothing       -> raiseError ("unboxInteger: can't find address "++showAddr a++" in heap")
                      Just (NNum i) -> return i
                      Just _        -> raiseError ("unboxInteger: non-integer")

primitive1 :: (b -> Monadic ()) -> (Addr -> Monadic a) -> (a->b) -> Monadic ()
primitive1 box unbox op = do stack <- getStack
                             case stackPop stack of
                               Nothing      -> raiseError ("primitive1: empty stack")
                               Just (as, a) -> do putStack as
                                                  i <- unbox a
                                                  box (op i)

primitive2 :: (b -> Monadic ()) -> (Addr -> Monadic a) -> (a->a->b) -> Monadic ()
primitive2 box unbox op = do stack <- getStack
                             case stackPop stack of
                               Nothing      -> raiseError ("primitive1: empty stack")
                               Just (s, a0) -> do case stackPop s of
                                                    Nothing      -> raiseError ("primitive1: empty stack")
                                                    Just (as, a1) -> do putStack as
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
       iStr "Steps taken = ", iNum (statGetSteps (gmStats s)),
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

