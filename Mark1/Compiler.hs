-------------------------------------------------------
-- G-Machine compiler
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------


module Compiler (
         compile
       ) where


import List
import LibAssoc
import LibMonadSE
import GlobalDefs
import CoreLang
import GMCode
import GMStack
import GMHeap
import GMGlobals
import GMStats
import GMState


newtype GMCompiledSC  = MkGMCompiledSC (Name, Int, GMCode)

type    GMCompiler    = CoreExpr -> Monadic GMCode

type    GMEnvironment = Assoc Name Int



-------------------------------------------------------
-- The monad
-------------------------------------------------------

type Monadic a = MSE (GMHeap, GMGlobals, GMEnvironment) a

runMonadic :: Monadic a -> a
runMonadic m = snd (runMSE m (heapInitial, globalInitial, undefined))

getHeap :: Monadic GMHeap
getHeap = do (h,g,e) <- readST
             return h

putHeap :: GMHeap -> Monadic ()
putHeap h' = do (h,g,e) <-  readST
                writeST (h',g,e)

getGlobals :: Monadic GMGlobals
getGlobals = do (h,g,e) <- readST
                return g

putGlobals :: GMGlobals -> Monadic ()
putGlobals g' = do (h,g,e) <-  readST
                   writeST (h,g',e)

runAt :: Monadic a -> GMEnvironment -> Monadic a
m `runAt` e' = do (h,g,e) <- readST
                  writeST (h,g,e')
                  a <- m
                  (h',g',_) <- readST
                  writeST (h',g',e)
                  return a

getEnvironment :: Monadic GMEnvironment
getEnvironment = do (h,g,e) <- readST
                    return e

-------------------------------------------------------


compile :: CoreProgram -> GMState
compile program = runMonadic (compileM program)

compileM :: CoreProgram -> Monadic GMState
compileM program = do buildInitialHeap program
                      heap <- getHeap
                      globals <- getGlobals          
                      code <- initialCode
                      return MkGMState { gmCode    = code,
                                         gmStack   = stackEmpty,
                                         gmHeap    = heap,
                                         gmGlobals = globals,
                                         gmStats   = statInitial }




preludeDefs :: CoreProgram
preludeDefs
  = MkProgram
    [ MkScDefn ("I", ["x"],               EVar "x"),
      MkScDefn ("K", ["x","y"],           EVar "x"),
      MkScDefn ("K1",["x","y"],           EVar "y"),
      MkScDefn ("S", ["f","g","x"],       EAp (EAp (EVar "f") (EVar "x"))
                                          (EAp (EVar "g") (EVar "x"))),
      MkScDefn ("compose", ["f","g","x"], EAp (EVar "f")
                                              (EAp (EVar "g") (EVar "x"))),
      MkScDefn ("twice", ["f"],           EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]



buildInitialHeap :: CoreProgram -> Monadic ()
buildInitialHeap program = do let MkProgram scDefs = program ++ preludeDefs
                              compiledSCs <- mapM compileSc scDefs
                              mapM allocateSc compiledSCs
                              return ()




allocateSc :: GMCompiledSC -> Monadic ()
allocateSc (MkGMCompiledSC (name, nargs, code)) = do heap <- getHeap
                                                     let (heap', addr) = heapAlloc heap (NGlobal nargs code)
                                                     putHeap heap'
                                                     globals <-  getGlobals
                                                     let globals' = globalAdd globals (name, addr)
                                                     putGlobals globals'


initialCode :: Monadic GMCode
initialCode = return (codeFromList [Pushglobal "main", Unwind])


compileSc :: CoreScDefn -> Monadic GMCompiledSC
compileSc (MkScDefn (name, env, body)) = do code <- compileR body `runAt` (aFromList (zip env [0..]))
                                            return (MkGMCompiledSC (name, length env, code))

compileR :: GMCompiler
compileR e = do ce <- compileC e 
                env <- getEnvironment
                return (ce ++ codeFromList [Slide (aLength env + 1), Unwind])

compileC :: GMCompiler
compileC (EVar v)    = do env <- getEnvironment
                          let Just n = aLookup env v 
                          if elem v (aDomain env) then return (codeFromList [Push n])
                                                  else return (codeFromList [Pushglobal v])

compileC (ENum n)    = return (codeFromList [Pushint n])
compileC (EAp e1 e2) = do ce2 <- compileC e2 
                          env <- getEnvironment
                          ce1 <- compileC e1 `runAt` (map (+1) env)
                          return (ce2 ++ ce1 ++ codeFromList [Mkap])


compiledPrimitives :: [GMCompiledSC]
compiledPrimitives = []
