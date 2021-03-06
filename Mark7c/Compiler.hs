-------------------------------------------------------
-- G-Machine compiler
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- Jos� E. Gallardo, December 1997
-------------------------------------------------------


-- ATENCI�N AL PUNTO 3.8.7
-- Ninguno de los dos problemas est� resuelto
-- 1) Ha de resolverse. Es importante
-- 2) No presenta problemas si se sigue un poco de disciplina
--    declarando cada constructor de dato de modo saturado


module Compiler (
         compile
       ) where

import Monad
import List
import LibAssoc
import LibMonadSE
import GlobalDefs
import CoreLang
import CoreLex
import GMCode
import GMSStack
import GMVStack
import GMDump
import GMHeap
import GMGlobals
import GMStats
import GMState
import ConstrTags

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


compile :: CoreProgram -> CoreProgram -> GMState
compile program prelude = runMonadic (compileM (prelude `mplus` program))

compileM :: CoreProgram -> Monadic GMState
compileM program = do buildInitialHeap program
                      heap <- getHeap
                      globals <- getGlobals          
                      code <- initialCode
                      return MkGMState { gmCode    = code,
                                         gmSStack  = sstackEmpty,
                                         gmVStack  = vstackEmpty,
                                         gmDump    = dumpEmpty,
                                         gmHeap    = heap,
                                         gmGlobals = globals,
                                         gmStats   = statInitial }



{-
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
      MkScDefn ("twice", ["f"],           EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ]
-}

buildInitialHeap :: CoreProgram -> Monadic ()
buildInitialHeap program = do let MkProgram scDefs = program  `mplus`  primitives --  `mplus`  preludeDefs 
                              compiledSCs <- mapM compileSc scDefs
                              mapM allocateSc (compiledSCs  `mplus`  compiledPrimitives)                              
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
compileR (ELet rec defs e) 
                                   | rec         = compileLetrec compileR defs (codeFromList []) e
                                   | otherwise   = compileLet    compileR defs (codeFromList []) e
                                   where n = length defs                                    
compileR e@(EAp (EAp (EAp (EVar op) e0) e1) e2)  
			           | op == tkIf  = do cE0 <- compileB e0
                                                      cE1 <- compileR e1
                                                      cE2 <- compileR e2
                                                      return (cE0  `mplus`  codeFromList [Cond cE1 cE2])
                                   | otherwise   =  defaultR e                 
compileR (ECase e alts)                          = do cE    <- compileE e
                                                      cAlts <- compileD compileAR alts
                                                      return (cE  `mplus`  codeFromList [Casejump cAlts])
compileR e                                       = defaultR e

defaultR :: GMCompiler
defaultR e                                       = do cE <- compileE e
                                                      env <- getEnvironment
                                                      let d = (aLength env)
                                                      return (cE  `mplus`  codeFromList [Update d, Pop d, Unwind])

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
compileC (EInt n)          = return (codeFromList [Pushint n])
compileC (EChar c)         = return (codeFromList [Pushchar c])
compileC e@(EAp e1 e2)     
 | isSaturatedConstr spine = compileCSaturatedConstr (reverse spine) 
 | isConstr e1             = raiseError ("can't compile expression: unsaturated constructor:\n"++(pprint e)++" ...")
 | otherwise               = do ce2 <- compileC e2 
                                env <- getEnvironment
                                ce1 <- compileC e1 `atEnvironment` (fmap (+1) env)
                                return (ce2  `mplus`  ce1  `mplus`  codeFromList [Mkap])
   where spine                            = mkSpine e
         isSaturatedConstr (EConstr t a:es) = a == length es
         isSaturatedConstr _                = False
         isConstr (EConstr _ _)             = True
         isConstr _                         = False
         mkSpine (EAp e1 e2)                = mkSpine e1  `mplus`  [e2]
         mkSpine e                          = [e]

compileC (ELet rec defs e) 
               | rec       = compileLetrec compileC defs (codeFromList [Slide n]) e
               | otherwise = compileLet    compileC defs (codeFromList [Slide n]) e 
               where n = length defs
compileC e@(ECase _ _)         = raiseError ("can't compile case expresion in non-strict context:\n"++(pprint e))

-- Takes as argument the spine
compileCSaturatedConstr :: [CoreExpr] -> Monadic GMCode
compileCSaturatedConstr [EConstr t a] = return (codeFromList [Pack t a])
compileCSaturatedConstr (e:es)        = do cE  <- compileC e 
                                           env <- getEnvironment
                                           cEs <- compileCSaturatedConstr es `atEnvironment` (fmap (+1) env)
                                           return (cE  `mplus`  cEs)

-------------------------------------------------------
-- Compiling lets
-------------------------------------------------------

compileLet :: GMCompiler -> [CoreDefn] -> GMCode -> GMCompiler
compileLet compile defs finalInstrs expr =
    do cDefs <- compileLetDefs defs
       env'  <- extendEnvWithDefs defs
       cExpr <- compile expr `atEnvironment` env'
       return (cDefs  `mplus`  cExpr  `mplus`  finalInstrs)
 

compileLetDefs :: [CoreDefn] -> Monadic GMCode
compileLetDefs []                        = return (codeFromList [])
compileLetDefs (MkDefn (name,expr):defs) = do cExpr <- compileC expr
                                              env   <- getEnvironment
                                              cDefs <- compileLetDefs defs `atEnvironment` (fmap (+1) env)
                                              return (cExpr  `mplus`  cDefs)

compileLetrec :: GMCompiler -> [CoreDefn] -> GMCode -> GMCompiler
compileLetrec compile defs finalInstrs expr =
    do env'  <- extendEnvWithDefs defs
       let n = length defs
       cDefs <- compileLetrecDefs defs (n-1) `atEnvironment` env'
       cExpr <- compile expr `atEnvironment` env'
       return (codeFromList [Alloc n]  `mplus`  cDefs  `mplus`  cExpr  `mplus`  finalInstrs)
 
compileLetrecDefs :: [CoreDefn] -> Int -> Monadic GMCode
compileLetrecDefs []                        n = return (codeFromList [])
compileLetrecDefs (MkDefn (name,expr):defs) n = do cExpr <- compileC expr
                                                   cDefs <- compileLetrecDefs defs (n-1)
                                                   return (cExpr  `mplus`  codeFromList [Update n]  `mplus`  cDefs)


-------------------------------------------------------
-- B Scheme
-------------------------------------------------------

compileB :: GMCompiler
compileB (EInt n)                               = return (codeFromList [Pushbasic (BasicInt n)])
compileB (EChar c)                              = return (codeFromList [Pushbasic (BasicChar c)])
compileB (ELet rec defs e) 
                                   | rec        = compileLetrec compileB defs (codeFromList [Pop n]) e
                                   | otherwise  = compileLet    compileB defs (codeFromList [Pop n]) e
                                   where n = length defs
compileB e@(EAp (EAp (EVar op) e0) e1)          = case aLookup builtinDyadic op of
                                                   Just i  -> do cE1 <- compileB e1
                                                                 cE0 <- compileB e0
                                                                 return (cE1  `mplus`  cE0  `mplus`  codeFromList [i])
                                                   Nothing -> defaultB e
compileB e@(EAp (EVar op) e1)                   = case aLookup builtinUnary op of
                                                   Just i  -> do cE <- compileB e1
                                                                 return (cE  `mplus`  codeFromList [i])
                                                   Nothing -> defaultB e                      
compileB e@(EAp (EAp (EAp (EVar op) e0) e1) e2) 
                                   | op == tkIf = do cE0 <- compileB e0
                                                     cE1 <- compileB e1
                                                     cE2 <- compileB e2
                                                     return (cE0  `mplus`  codeFromList [Cond cE1 cE2])
                                   | otherwise  = defaultB e
compileB e                                      = defaultB e

defaultB :: GMCompiler                                                     
defaultB e                                      = do cE <- compileE e
                                                     return (cE  `mplus`  codeFromList [Get])


-------------------------------------------------------
-- E Scheme
-------------------------------------------------------

compileE :: GMCompiler
compileE (EInt n)                               = return (codeFromList [Pushint n])
compileE (EChar c)                              = return (codeFromList [Pushchar c])
compileE (ELet rec defs e) 
                                   | rec        = compileLetrec compileE defs (codeFromList [Slide n]) e
                                   | otherwise  = compileLet    compileE defs (codeFromList [Slide n]) e
                                   where n = length defs                                    
compileE (ECase e alts)                         = do cE    <- compileE e
                                                     cAlts <- compileD compileAE alts
                                                     return (cE  `mplus`  codeFromList [Casejump cAlts])
compileE e@(EAp (EAp (EVar op) e0) e1)          
                              | op == tkStrict  = do ce1 <- compileE e1 
                                                     env <- getEnvironment
                                                     ce0 <- compileC e0 `atEnvironment` (fmap (+1) env)
                                                     return (ce1  `mplus`  ce0 `mplus`  codeFromList [Mkap,Eval])
		              | otherwise       = case aLookup builtinDyadic op of
                                                    Just i  -> do cE <- compileB e
                                                                  i  <- mkbasic op
                                                                  return (cE  `mplus`  codeFromList [i])
                                                    Nothing -> defaultE e
compileE e@(EAp (EVar op) e1)                   = case aLookup builtinUnary op of
                                                   Just i  -> do cE <- compileB e
                                                                 i  <- mkbasic op
                                                                 return (cE  `mplus`  codeFromList [i])
                                                   Nothing -> defaultE e                       
compileE e@(EAp (EAp (EAp (EVar op) e0) e1) e2) 
                                   | op == tkIf = do cE0 <- compileB e0
                                                     cE1 <- compileE e1
                                                     cE2 <- compileE e2
                                                     return (cE0  `mplus`  codeFromList [Cond cE1 cE2])
                                   | otherwise  = defaultE e
compileE e					= defaultE e               

defaultE :: GMCompiler
defaultE e                                      = do cE <- compileC e
                                                     return (cE  `mplus`  codeFromList [Eval])

                               
-------------------------------------------------------
-- D Scheme
-------------------------------------------------------

compileD :: (CoreAlter -> Monadic (Int,GMCode)) -> [CoreAlter] -> Monadic [(Int, GMCode)]
compileD compile alts =  (sequence (map (compile) alts))


-------------------------------------------------------
-- A Scheme
-------------------------------------------------------

compileAE :: CoreAlter -> Monadic (Int,GMCode)
compileAE (MkAlter (t, xs, body)) = do let n = length xs
                                       env' <- extendEnvWithNames xs  
                                       cBody <- compileE body `atEnvironment` env'
                                       return (t, codeFromList [Split n]  `mplus`  cBody  `mplus`  codeFromList [Slide n])


compileAR :: CoreAlter -> Monadic (Int,GMCode)
compileAR (MkAlter (t, xs, body)) = do let n = length xs
                                       env' <- extendEnvWithNames xs  
                                       cBody <- compileR body `atEnvironment` env'
                                       return (t, codeFromList [Split n]  `mplus`  cBody)

-------------------------------------------------------
-- Extending environments
-------------------------------------------------------

extendEnvWithDefs :: [CoreDefn] -> Monadic GMEnvironment
extendEnvWithDefs defs = do env <- getEnvironment
                            let n = length defs
                            let extractName = \(MkDefn (name, expr)) -> name
                            return (fmap (+n) env  `mplus`  aFromList (zip (map extractName defs) [n-1, n-2 .. 0]))

extendEnvWithNames :: [Name] -> Monadic GMEnvironment
extendEnvWithNames names = do env <- getEnvironment
                              let n = length names
                              return (fmap (+n) env  `mplus`  aFromList (zip names [0, 1 .. n-1]))


-------------------------------------------------------

-- Strict builtin dyadic functions
builtinDyadic :: Assoc Name Instruction
builtinDyadic = aFromList [(tkAdd,IAdd), (tkSub, ISub),  (tkMul,IMul), 
                           (tkDiv,IDiv), (tkLT,  ILT),   (tkLE, ILE),  
                           (tkEQT,IEQT), (tkNEQT,INEQT), (tkGT, IGT),  
                           (tkGE, IGE) ]  

-- Strict builtin Unary functions
builtinUnary :: Assoc Name Instruction     
builtinUnary = aFromList [(tkNegate,INegate),
                           (tkOrd,COrd), (tkChr,CChr)]
                            

-- True, if primitive returns a boolean value
returnsBool :: Name -> Bool
returnsBool x = x `elem` [tkLT, tkLE, tkEQT, tkNEQT, tkGT, tkGE]

-- True, if primitive returns a integer value
returnsInt :: Name -> Bool
returnsInt x = x `elem` [tkAdd, tkSub, tkMul, tkDiv, tkNegate, tkOrd]

-- True, if primitive returns a character value
returnsChar :: Name -> Bool
returnsChar x = x `elem` [tkChr]

mkbasic :: Name -> Monadic Instruction
mkbasic op = if returnsBool op then return Mkbool
                               else if returnsInt  op then return Mkint
                               else if returnsChar op then return Mkchar
                               else raiseError ("Unknown return type for primitive: "++op)


primitives :: CoreProgram
primitives 
  = MkProgram [ -- Integers
                mkDiadic tkAdd, 
                mkDiadic tkSub, 
                mkDiadic tkMul, 
                mkDiadic tkDiv, 
                mkDiadic tkLT,  
                mkDiadic tkLE,  
                mkDiadic tkEQT, 
                mkDiadic tkNEQT,
                mkDiadic tkGT,  
                mkDiadic tkGE,
                mkUnaric tkNegate,
                -- Booleans
                MkScDefn (tkIf,     ["c","t","f"], (EAp (EAp (EAp (EVar tkIf) (EVar "c")) (EVar "t")) (EVar "f"))),
                MkScDefn (tkAnd,    ["x","y"],     ECase (EVar "x") [MkAlter (tagFalse, [], EVar tkFalse),
                					             MkAlter (tagTrue,  [], EVar "y")
                                                   ]),
                MkScDefn (tkOr,     ["x","y"],     ECase (EVar "x") [MkAlter (tagFalse, [], EVar "y"),
                					             MkAlter (tagTrue,  [], EVar tkTrue)
                                                   ]),
                -- Characters                                   
                mkUnaric tkOrd,
                mkUnaric tkChr
    ]           
  

    
mkDiadic :: Name -> CoreScDefn
mkDiadic op = MkScDefn (op, ["x","y"], (EAp (EAp (EVar op) (EVar "x")) (EVar "y")))
  
mkUnaric :: Name -> CoreScDefn
mkUnaric op = MkScDefn (op, ["x"], (EAp (EVar op) (EVar "x")))

compiledPrimitives :: [GMCompiledSC]
compiledPrimitives = [
	MkGMCompiledSC ("seq", 2,
                                   codeFromList [Push 0, Eval, Push 2, Eval, Update 2, Pop 2, Unwind ])
        ]

