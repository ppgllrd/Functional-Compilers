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
                                               gmHeap    = gmHeap st,
                                               gmGlobals = gmGlobals st,
                                               gmStats   = gmStats' }
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
doPush n = do heap <- getHeap
              stack <- getStack
              case stackSelect stack (n+1) of
                Nothing      -> raiseError ("doPush: stack too small")
                Just anPlus1 -> case heapLookup heap anPlus1 of
                                  Nothing   -> raiseError ("doPush: can't find address " ++show anPlus1)
                                  Just node -> do a <- getArg node
                                                  let newStack = stackPush stack a
                                                  putStack newStack
              
getArg :: Node -> Monadic Addr
getArg (NAp a1 a2) = return a2
getArg _           = raiseError ("getArg: non NAp node")



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
                where newState (NNum _)      = return ()
                      newState (NAp a1 a2)   = do stack <- getStack
                                                  let newStack = stackPush stack a1
                                                  putStack newStack
                                                  putCode (codeFromList [Unwind])
                      newState (NGlobal n c) = do stack <- getStack
                                                  if stackLength stack <= n then raiseError "Unwinding with too few arguments. Type error"
                                                                            else putCode c
                      newState (NInd a)      = do stack <- getStack
                                                  let Just (stack', a0) = stackPop stack
                                                  let stack'' = stackPush stack' a
                                                  putStack stack'' 
                                                  code <- getCode
                                                  if codeIsNull code then putCode (codeFromList [Unwind])
                                                                     else raiseError ("doUnwind: code should be empty")
     
                                                

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


-- showing the machine state


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
       iStack s, iNewline,
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




