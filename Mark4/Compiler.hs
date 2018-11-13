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
import CoreLex
import GMCode
import GMStack
import GMDump
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

atEnvironment :: Monadic a -> GMEnvironment -> Monadic a
m `atEnvironment` e' = do (h,g,e) <- readST
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
                                         gmDump    = dumpEmpty,
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
                              mapM allocateSc (compiledSCs ++ compiledPrimitives)
                              return ()




allocateSc :: GMCompiledSC -> Monadic ()
allocateSc (MkGMCompiledSC (name, nargs, code)) = do heap <- getHeap
                                                     let (heap', addr) = heapAlloc heap (NGlobal nargs code)
                                                     putHeap heap'
                                                     globals <-  getGlobals
                                                     let globals' = globalAdd globals (name, addr)
                                                     putGlobals globals'


initialCode :: Monadic GMCode
initialCode = return (codeFromList [Pushglobal "main", Eval])


compileSc :: CoreScDefn -> Monadic GMCompiledSC
compileSc (MkScDefn (name, env, body)) = do code <- compileR body `atEnvironment` (aFromList (zip env [0..]))
                                            return (MkGMCompiledSC (name, length env, code))

compileR :: GMCompiler
compileR e = do ce <- compileC e 
                env <- getEnvironment
                let d = (aLength env)
                return (ce ++ codeFromList [Update d, Pop d, Unwind])

compileC :: GMCompiler
compileC (EVar v)          = do env <- getEnvironment
                                let Just n = aLookup env v 
                                if elem v (aDomain env) then return (codeFromList [Push n])
                                                        else return (codeFromList [Pushglobal v])
compileC (ENum n)          = return (codeFromList [Pushint n])
compileC (EAp e1 e2)       = do ce2 <- compileC e2 
                                env <- getEnvironment
                                ce1 <- compileC e1 `atEnvironment` (map (+1) env)
                                return (ce2 ++ ce1 ++ codeFromList [Mkap])
compileC (ELet rec defs e) 
               | rec       = compileLetrec compileC defs e
               | otherwise = compileLet    compileC defs e


compileLet :: GMCompiler -> [CoreDefn] -> GMCompiler
compileLet compile defs expr = do cDefs <- compileLet' defs
                                  env'  <- compileArgs defs
                                  cExpr <- compile expr `atEnvironment` env'
                                  let n = length defs
                                  return (cDefs ++ cExpr ++ codeFromList [Slide n])
 

compileLet' :: [CoreDefn] -> Monadic GMCode
compileLet' []                        = return (codeFromList [])
compileLet' (MkDefn (name,expr):defs) = do cExpr <- compileC expr
                                           env   <- getEnvironment
                                           cDefs <- compileLet' defs `atEnvironment` (map (+1) env)
                                           return (cExpr ++ cDefs)

compileLetrec :: GMCompiler -> [CoreDefn] -> GMCompiler
compileLetrec compile defs expr = do env'  <- compileArgs defs
                                     let n = length defs
                                     cDefs <- compileLetrec' defs (n-1) `atEnvironment` env'
                                     cExpr <- compile expr `atEnvironment` env'
                                     return (codeFromList [Alloc n] ++ cDefs ++ cExpr ++ codeFromList [Slide n])
 
compileLetrec' :: [CoreDefn] -> Int -> Monadic GMCode
compileLetrec' []                        n = return (codeFromList [])
compileLetrec' (MkDefn (name,expr):defs) n = do cExpr <- compileC expr
                                                cDefs <- compileLetrec' defs (n-1)
                                                return (cExpr ++ codeFromList [Update n] ++ cDefs)



compileArgs :: [CoreDefn] -> Monadic GMEnvironment
compileArgs defs = do env <- getEnvironment
                      let n = length defs
                      let extractName = \(MkDefn (name, expr)) -> name
                      return (aFromList (zip (map extractName defs) [n-1, n-2 .. 0]) ++ map (+n) env)


compiledPrimitives :: [GMCompiledSC]
compiledPrimitives = [mkDiadic tkAdd  IAdd,
                      mkDiadic tkSub  ISub,
                      mkDiadic tkMul  IMul,
                      mkDiadic tkDiv  IDiv,
                      mkDiadic tkLT   ILT, 
                      mkDiadic tkLE   ILE,
                      mkDiadic tkEQT  IEQT,
                      mkDiadic tkNEQT INEQT,
                      mkDiadic tkGT   IGT,
                      mkDiadic tkGE   IGE,
                      MkGMCompiledSC ("negate", 1,
                                      codeFromList [ Push 0, Eval, INegate,
                                                     Update 1, Pop 1, Unwind ]),
                      MkGMCompiledSC ("if", 3,
                                      codeFromList [ Push 0, Eval, Cond (codeFromList [Push 1])
                                                                        (codeFromList [Push 2]),
                                                     Update 3, Pop 3, Unwind ])
                     ]


mkDiadic :: Name -> Instruction -> GMCompiledSC
mkDiadic name op = MkGMCompiledSC (name, 2,
                                   codeFromList [ Push 1, Eval, Push 1, Eval, op,
                                                  Update 2, Pop 2, Unwind ])

mkUnary :: Name -> Instruction -> GMCompiledSC
mkUnary name op = MkGMCompiledSC (name, 1,
                                  codeFromList [ Push 0, Eval, op,
                                                 Update 1, Pop 1, Unwind ])

