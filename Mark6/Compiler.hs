-------------------------------------------------------
-- G-Machine compiler
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------


-- ATENCIÓN AL PUNTO 3.8.7
-- Ninguno de los dos problemas está resuelto
-- 1) Ha de resolverse. Es importante
-- 2) No presenta problemas si se sigue un poco de disciplina
--    declarando cada constructor de dato de modo saturado


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
      MkScDefn ("twice", ["f"],           EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")),
      MkScDefn ("False",  [],             EConstr 1 0),
      MkScDefn ("True",   [],             EConstr 2 0)
    ]



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
initialCode = return (codeFromList [Pushglobal "main", Eval, Print])


-------------------------------------------------------
-- Sc Scheme
-------------------------------------------------------

compileSc :: CoreScDefn -> Monadic GMCompiledSC
compileSc (MkScDefn (name, env, body)) = do code <- compileR body `atEnvironment` (aFromList (zip env [0..]))
                                            return (MkGMCompiledSC (name, length env, code))

-------------------------------------------------------
-- R Scheme
-------------------------------------------------------

compileR :: GMCompiler
compileR e = do ce <- compileE e 
                env <- getEnvironment
                let d = (aLength env)
                return (ce ++ codeFromList [Update d, Pop d, Unwind])


-------------------------------------------------------
-- C Scheme
-------------------------------------------------------

compileC :: GMCompiler
compileC (EVar v)          = do env <- getEnvironment
                                let Just n = aLookup env v 
                                if elem v (aDomain env) then return (codeFromList [Push n])
                                                        else return (codeFromList [Pushglobal v])
compileC (EConstr t 0)     = return (codeFromList [Pack t 0]) 
compileC e@(EConstr t _)   = raiseError ("can't compile expression: unsaturated constructor:\n"++(pprint e)++" ...")
compileC (ENum n)          = return (codeFromList [Pushint n])
compileC e@(EAp e1 e2)     
     | isSaturatedConstr spine = compileCSaturatedConstr (reverse spine) 
     | isConstr e1             = raiseError ("can't compile expression: unsaturated constructor:\n"++(pprint e)++" ...")
     | otherwise               = do ce2 <- compileC e2 
                                    env <- getEnvironment
                                    ce1 <- compileC e1 `atEnvironment` (map (+1) env)
                                    return (ce2 ++ ce1 ++ codeFromList [Mkap])
   where spine                            = mkSpine e
         isSaturatedConstr (EConstr t a:es) = a == length es
         isSaturatedConstr _                = False
         isConstr (EConstr _ _)             = True
         isConstr _                         = False
         mkSpine (EAp e1 e2)                = mkSpine e1 ++ [e2]
         mkSpine e                          = [e]

compileC (ELet rec defs e) 
               | rec       = compileLetrec compileC defs e
               | otherwise = compileLet    compileC defs e
compileC e@(ECase _ _)         = raiseError ("can't compile case expresion in non-strict context:\n"++(pprint e))

-- Takes as argument the spine
compileCSaturatedConstr :: [CoreExpr] -> Monadic GMCode
compileCSaturatedConstr [EConstr t a] = return (codeFromList [Pack t a])
compileCSaturatedConstr (e:es)        = do cE  <- compileC e 
                                           env <- getEnvironment
                                           cEs <- compileCSaturatedConstr es `atEnvironment` (map (+1) env)
                                           return (cE ++ cEs)

-------------------------------------------------------
-- Compiling lets
-------------------------------------------------------

compileLet :: GMCompiler -> [CoreDefn] -> GMCompiler
compileLet compile defs expr = do cDefs <- compileLetDefs defs
                                  env'  <- extendEnvWithDefs defs
                                  cExpr <- compile expr `atEnvironment` env'
                                  let n = length defs
                                  return (cDefs ++ cExpr ++ codeFromList [Slide n])
 

compileLetDefs :: [CoreDefn] -> Monadic GMCode
compileLetDefs []                        = return (codeFromList [])
compileLetDefs (MkDefn (name,expr):defs) = do cExpr <- compileC expr
                                              env   <- getEnvironment
                                              cDefs <- compileLetDefs defs `atEnvironment` (map (+1) env)
                                              return (cExpr ++ cDefs)

compileLetrec :: GMCompiler -> [CoreDefn] -> GMCompiler
compileLetrec compile defs expr = do env'  <- extendEnvWithDefs defs
                                     let n = length defs
                                     cDefs <- compileLetrecDefs defs (n-1) `atEnvironment` env'
                                     cExpr <- compile expr `atEnvironment` env'
                                     return (codeFromList [Alloc n] ++ cDefs ++ cExpr ++ codeFromList [Slide n])
 
compileLetrecDefs :: [CoreDefn] -> Int -> Monadic GMCode
compileLetrecDefs []                        n = return (codeFromList [])
compileLetrecDefs (MkDefn (name,expr):defs) n = do cExpr <- compileC expr
                                                   cDefs <- compileLetrecDefs defs (n-1)
                                                   return (cExpr ++ codeFromList [Update n] ++ cDefs)



-------------------------------------------------------
-- E Scheme
-------------------------------------------------------

compileE :: GMCompiler
compileE (ENum n)                               = return (codeFromList [Pushint n])
compileE (ELet rec defs e) 
                                    | rec       = compileLetrec compileE defs e
                                    | otherwise = compileLet    compileE defs e
compileE e@(EAp (EAp (EVar op) e0) e1)          = case aLookup builtInDyadic op of
                                                   Just i  -> do env <- getEnvironment
                                                                 cE1 <- compileE e1
                                                                 cE0 <- compileE e0 `atEnvironment` (map (+1) env)
                                                                 return (cE1 ++ cE0 ++ codeFromList [i])
                                                   Nothing -> do cE <- compileC e
                                                                 return (cE ++ codeFromList [Eval])
compileE (EAp (EVar "negate") e)                = do cE <- compileE e
                                                     return (cE ++ codeFromList [INegate])

compileE (EAp (EAp (EAp (EVar "if") e0) e1) e2) = do cE0 <- compileE e0
                                                     cE1 <- compileE e1
                                                     cE2 <- compileE e2
                                                     return (cE0 ++ codeFromList [Cond cE1 cE2])

compileE (ECase e alts)                         = do cE    <- compileE e
                                                     cAlts <- compileD compileA alts
                                                     return (cE ++ codeFromList [Casejump cAlts])
compileE e                                      = do cE <- compileC e
                                                     return (cE ++ codeFromList [Eval])


-------------------------------------------------------
-- D Scheme
-------------------------------------------------------

compileD :: (CoreAlter -> Monadic (Int,GMCode)) -> [CoreAlter] -> Monadic [(Int, GMCode)]
compileD compile alts =  (accumulate (map compile alts))


-------------------------------------------------------
-- A Scheme
-------------------------------------------------------

compileA :: CoreAlter -> Monadic (Int,GMCode)
compileA (MkAlter (t, xs, body)) = do let n = length xs
                                      env' <- extendEnvWithNames xs  
                                      cBody <- compileE body `atEnvironment` env'
                                      return (t, codeFromList [Split n] ++ cBody ++ codeFromList [Slide n])


-------------------------------------------------------
-- Extending environments
-------------------------------------------------------

extendEnvWithDefs :: [CoreDefn] -> Monadic GMEnvironment
extendEnvWithDefs defs = do env <- getEnvironment
                            let n = length defs
                            let extractName = \(MkDefn (name, expr)) -> name
                            return (map (+n) env ++ aFromList (zip (map extractName defs) [n-1, n-2 .. 0]))

extendEnvWithNames :: [Name] -> Monadic GMEnvironment
extendEnvWithNames names = do env <- getEnvironment
                              let n = length names
                              return (map (+n) env ++ aFromList (zip names [0, 1 .. n-1]))


-------------------------------------------------------


builtInDyadic :: Assoc Name Instruction
builtInDyadic = aFromList [(tkAdd,IAdd), (tkSub, ISub),  (tkMul,IMul), 
                           (tkDiv,IDiv), (tkLT,  ILT),   (tkLE, ILE),  
                           (tkEQT,IEQT), (tkNEQT,INEQT), (tkGT, IGT),  
                           (tkGE, IGE)]  


-- Must remain, to be used in non strict contexts
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

