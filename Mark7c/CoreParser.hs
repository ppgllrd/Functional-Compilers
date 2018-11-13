-------------------------------------------------------
-- A Parser for the Core Language (see Grammar.txt) 
-- defined in
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------


module CoreParser (
         parse
       ) where

import Monad
import CoreLex
import CoreLang
import LibParser

-------------------------------------------------------
-- Lexical parsers 
-------------------------------------------------------

pComment  :: Parser s ()
pComment = 
   do pStr "--"
      many (pSatisfy (/='\n'))
      return ()
  `mplus`  
   do pStr "{-"
      pSkipComment  `mplus`  fatalError "Non closed comment"   
      return ()
       where 
        pSkipComment = 
         do many (pSatisfy (/='-')) 
            '-' <- readAChar          
            x   <- lookupAChar   
            if x == '}' then readAChar
                        else pSkipComment
       

pJunk :: Parser s ()
pJunk = do many (pSpaces  `mplus`  pComment)
           return ()


pToken          :: Parser s a -> Parser s a
pToken p = do v <- p
              pJunk
              return v


pInteger  :: Parser s Int
pInteger  = pToken pNat


pCharacter      :: Parser s Char
pCharacter      = do pLiteral tkApos
                     c <- readAChar
                     pMustBe tkApos
                     return c


pString         :: Parser s CoreExpr
pString         = do pLiteral tkQuotes
                     xs <- many (pSatisfy (/=(head tkQuotes)))
                     pMustBe tkQuotes
                     let mkCons x xs = EAp (EAp (EVar tkCons) (EChar x) ) xs
                     return (foldr mkCons (EVar tkNil) xs)


pLiteral   :: String -> Parser s String
pLiteral xs   = pToken (pStr xs)


pVar     :: Parser s String
pVar    = pToken (do x <- pVar'
                     guard (not (elem x keyWords))
                     return x)
             where pVar' :: Parser s String
                   pVar'= do x  <- pLetter
                             xs <- many (pLetter  `mplus`  pDigit  `mplus`  pChar '_')
                             return (x:xs)


pRelOp     :: Parser s String
pRelOp     = do       
                      pLiteral tkLE         -- The order is important 
             `mplus`  pLiteral tkLT         -- Longest tokens before
             `mplus`  pLiteral tkEQT
             `mplus`  pLiteral tkNEQT 
             `mplus`  pLiteral tkGE  
             `mplus`  pLiteral tkGT 



pMustBe        :: String -> Parser s String
pMustBe str     = pLiteral str  `mplus`  fatalError ("'" ++ str ++ "'" ++ " expected")


-------------------------------------------------------
-- Program Grammar
-------------------------------------------------------

pProgram   :: Parser s CoreProgram
pProgram   = do pJunk 
                scs <- pSc `sepby1` pLiteral tkSemicolon
                return (MkProgram scs)


pSc        :: Parser s CoreScDefn
pSc        = do var  <- pVar
                vars <- many pVar
                pLiteral tkEq
                expr <- pExpr0  `mplus`  fatalError "Expression expected"
                return (MkScDefn (var, vars, expr))


pDefns    :: Parser s [CoreDefn]
pDefns    = do pDefn `sepby1` pLiteral tkSemicolon


pDefn     :: Parser s CoreDefn
pDefn     = do var <- pVar
               pLiteral tkEq
               expr <- pExpr0  `mplus`  fatalError "Expression expected"
               return (MkDefn (var, expr))


pAlts     :: Parser s [CoreAlter]
pAlts     = do pAlt `sepby1` pLiteral tkSemicolon
             

pAlt      :: Parser s CoreAlter
pAlt      = do pLiteral tkLT
               int <- pInteger     `mplus`  fatalError "Integer for tag expected"  
               pMustBe tkGT
               vars <- many pVar   `mplus`  fatalError "Variables expected" 
               pMustBe tkArrow
               expr <- pExpr0      `mplus`  fatalError "Expression expected"
               return (MkAlter (int, vars, expr))


pExpr0     :: Parser s CoreExpr
pExpr0     = do 
                      pLetrec  -- Order is important
             `mplus`  pLet 
             `mplus`  pCase 
             `mplus`  pIf
             `mplus`  pLambda 
             `mplus`  pExpr1 


pExpr1     :: Parser s CoreExpr
pExpr1 = do  
  e2 <- pExpr2
  mplus (do pLiteral tkOr
            e1 <- pExpr1
            return (EAp (EAp (EVar tkOr) e2) e1)) 
        (return e2)
         

pExpr2          :: Parser s CoreExpr
pExpr2 = do 
  e3 <- pExpr3
  mplus (do pLiteral tkAnd
            e2 <- pExpr2
            return (EAp (EAp (EVar tkAnd) e3) e2))
        (return e3)
         

pExpr3          :: Parser s CoreExpr
pExpr3 = do 
  e4 <- pExpr4
  mplus (do relOp <- pRelOp
            e4'   <- pExpr4
            return (EAp (EAp (EVar relOp) e4) e4'))
        (return e4)
         

pExpr4          :: Parser s CoreExpr
pExpr4 =  do 
  e5 <- pExpr5
  mplus (do pLiteral tkAdd
            e4  <- pExpr4
            return (EAp (EAp (EVar tkAdd) e5) e4))
        (mplus (do pLiteral tkSub
                   e5' <- pExpr5
                   return (EAp (EAp (EVar tkSub) e5) e5')) 
        (return e5))


pExpr5          :: Parser s CoreExpr
pExpr5          = do 
  e6 <- pExpr6
  mplus (do pLiteral tkMul
            e5  <- pExpr5
            return (EAp (EAp (EVar tkMul) e6) e5))
        (mplus (do pLiteral tkDiv
                   e6' <- pExpr6
                   return (EAp (EAp (EVar tkDiv) e6) e6'))
        (return e6))

pExpr6          :: Parser s CoreExpr
pExpr6          = do exprs <- many1 pExpr7
                     return (foldl1 EAp exprs)

  
pExpr7 :: Parser s CoreExpr
pExpr7 =   do var <- pVar
              return (EVar var)
         `mplus` 
           do int <- pInteger
              return (EInt int)
         `mplus` 
           do char <- pCharacter
              return (EChar char)      
         `mplus` 
           do pLiteral tkPack
              pLiteral tkOCB
              n1 <- pInteger    `mplus`  fatalError "Pack definition: Integer for tag expected"
              pMustBe tkComma
              n2 <- pInteger    `mplus`  fatalError "Pack definition: Integer for arity expected"
              pMustBe tkCCB
              return (EConstr n1 n2)
         `mplus`
           do pLiteral tkOL
              xs <- pExpr0 `sepby` (pLiteral tkComma)
              pLiteral tkCL
              let mkCons x xs = EAp (EAp (EVar tkCons) x ) xs
              return (foldr mkCons (EVar tkNil) xs)
         `mplus`
           do pLiteral tkOB
              e1 <- pExpr0
              pLiteral tkComma
              e2 <- pExpr0                  
              pLiteral tkComma
              e3 <- pExpr0                  
              pLiteral tkCB      
              let mkTuple3 a b c = EAp (EAp (EAp (EVar tkTuple3) a ) b) c
              return (mkTuple3 e1 e2 e3)
         `mplus`
           do pLiteral tkOB
              e1 <- pExpr0
              pLiteral tkComma
              e2 <- pExpr0                  
              pLiteral tkCB      
              let mkTuple2 a b = EAp (EAp (EVar tkTuple2) a ) b                  
              return (mkTuple2 e1 e2)
         `mplus` 
           do pLiteral tkOB
              e <- pExpr0   
              pMustBe tkCB
              return e
         `mplus` 
           do pString
            

pLet     :: Parser s CoreExpr
pLet     = do pLiteral tkLet
              pMustBe tkOCB
              defns <- pDefns  `mplus`  fatalError "Definitions expected"
              pMustBe tkCCB
              pMustBe tkIn
              expr <- pExpr0   `mplus`  fatalError "Expression expected"
              return (ELet nonRecursive defns expr)


pLetrec   :: Parser s CoreExpr
pLetrec =   do pLiteral tkLetrec
               pMustBe tkOCB
               defns <- pDefns  `mplus`  fatalError "Definitions expected"
               pMustBe tkCCB
               pMustBe tkIn
               expr <- pExpr0   `mplus`  fatalError "Expression expected"
               return (ELet recursive defns expr)


pCase     :: Parser s CoreExpr
pCase    = do pLiteral tkCase
              expr <- pExpr0  `mplus`  fatalError "Expression expected"
              pMustBe tkOf
              pMustBe tkOCB
              alts <- pAlts   `mplus`  fatalError "Alternatives expected"
              pMustBe tkCCB
              return (ECase expr alts)

pIf    :: Parser s CoreExpr
pIf = do pLiteral tkIf
         b <-  pExpr0  `mplus`  fatalError "Boolean expression expected"
         pMustBe tkThen
         t <- pExpr0   `mplus`  fatalError "Expression expected"
         pMustBe tkElse
         f <- pExpr0   `mplus`  fatalError "Expression expected"
         return (EAp (EAp (EAp (EVar tkIf) b) t) f)

pLambda    :: Parser s CoreExpr
pLambda    = do pLiteral tkLambda
                vars <- many1 pVar  `mplus`  fatalError "Variables expected"
                pMustBe tkDot
                expr <- pExpr0      `mplus`  fatalError "Expression expected"
                return (ELam vars expr)






parse :: String -> (CoreProgram, ())
parse str = runParser pProgram str ()




