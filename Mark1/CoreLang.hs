-------------------------------------------------
--
-- Types for the abstract grammar of the language
-- and functions for pretty printing (see Grammar.txt)
--
--  "Implementing Functional languages"
--
-- José E. Gallardo, December 1997
-------------------------------------------------

module CoreLang (
         IsRec, recursive, nonRecursive,
         Tag,
         bindersOf, rhssOf,
         pprExpr, pprProgram,
         Program(..), ScDefn(..), Expr(..), Alter(..), Defn(..),
         CoreProgram, CoreScDefn, CoreExpr, CoreAlter, CoreDefn,
         pprint
       ) where


import GlobalDefs
import CoreLex
import LibPretty


type IsRec        = Bool
type Tag          = Int

newtype ScDefn a  = MkScDefn (Name, [a], Expr a)
newtype Program a = MkProgram [ScDefn a]
newtype Defn a    = MkDefn (a, Expr a)
newtype Alter a   = MkAlter (Tag, [a], Expr a)

instance Monad Program 

instance MonadZero Program where
  -- zero :: Program a
  zero = MkProgram []

instance MonadPlus Program where
  -- (++) :: Program a -> Program a -> Program a
  MkProgram sc1 ++ MkProgram sc2 = MkProgram (sc1++sc2)


data Expr a
  =  EVar Name                     -- Variables
   | ENum Int                      -- Numbers
   | EConstr Tag Int               -- Constructor tag arity
   | EAp (Expr a) (Expr a)         -- Applications
   | ELet                          -- Let(rec) expressions
        IsRec                      --   boolean with True = recursive,
        [Defn a]                   --   Definitions
        (Expr a)                   --   Body of let(rec)
   | ECase                         -- Case expression
        (Expr a)                   --   Expression to scrutinise
        [Alter a]                  --   Alternatives
   | ELam [a] (Expr a)             -- Lambda abstractions


type CoreExpr    = Expr Name
type CoreScDefn  = ScDefn Name
type CoreProgram = Program Name
type CoreDefn    = Defn Name
type CoreAlter   = Alter Name



recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [Defn a] -> [a]
bindersOf defns = [name | MkDefn (name, rhs) <- defns]

rhssOf :: [Defn a] -> [Expr a]
rhssOf defns    = [rhs  | MkDefn (name, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False


-- This is a hack!! to remove "" from (show x::String)
myShow :: Show a => a -> String
myShow x = clean (show x)
  where clean []       = []
        clean ('"':xs) = clean1 xs
        clean xs       = xs
        clean1 []      = []
        clean1 ['"']   = []
        clean1 (x:xs)  = x:clean1 xs

-------------------------------------------------
-- Pretty printing Core expressions and Programs
-------------------------------------------------

type Prec = Int

parenIf :: Bool -> Iseq -> Iseq
parenIf hasParen iSeq | hasParen  = iConcat [iStr tkOB, iSeq, iStr tkCB] 
	 	      | otherwise = iSeq

pprOper :: Show a => Expr a -> String -> Expr a -> Bool -> Prec -> Prec -> Iseq 
pprOper  eLeft op eRight hasParen pLeft pRight =
  parenIf hasParen (iConcat [pprExpr eLeft pLeft, iStr " ",
                             iStr op, iStr " ",
                             pprExpr eRight pRight])


pprExpr :: Show a => Expr a -> Prec -> Iseq
pprExpr (EVar v)                    p =
  parenIf (p>7) (iStr v)
pprExpr (ENum n)                    p =
  parenIf (p>7) (iNum n)
pprExpr (EConstr tag arity)         p = 
  parenIf (p>7) (iConcat [iStr tkPack, iStr " ", iStr tkOCB, 
                          iNum tag, iStr tkComma, iNum arity, iStr tkCCB])
pprExpr (EAp (EAp (EVar op) e1) e2) p 
		      | op == tkOr    = pprOper e1 op e2 (p>1) 2 1 
		      | op == tkAnd   = pprOper e1 op e2 (p>2) 3 2
		      | op == tkGT    = pprOper e1 op e2 (p>3) 4 4
		      | op == tkGE    = pprOper e1 op e2 (p>3) 4 4
		      | op == tkEQT   = pprOper e1 op e2 (p>3) 4 4
		      | op == tkNEQT  = pprOper e1 op e2 (p>3) 4 4
		      | op == tkLT    = pprOper e1 op e2 (p>3) 4 4
		      | op == tkLE    = pprOper e1 op e2 (p>3) 4 4
		      | op == tkPlus  = pprOper e1 op e2 (p>4) 5 4
		      | op == tkMinus = pprOper e1 op e2 (p>4) 5 5
        	      | op == tkTimes = pprOper e1 op e2 (p>5) 6 5
		      | op == tkDiv   = pprOper e1 op e2 (p>5) 6 6
		      | otherwise     = parenIf (p>6) (iConcat [pprExpr (EAp (EVar op) e1) (7-1),
                                                                iStr " ",
                                                                pprExpr e2 7])
                                          -- (7-1) makes non atomic expressions 
                                          -- apear parenthesised

pprExpr (EAp e1 e2)                 p = 
  parenIf (p>6) (iConcat [pprExpr e1 (7-1), iStr " ", pprExpr e2 7 ])
  -- (7-1) makes non atomic expressions 
  -- apear parenthesised
pprExpr (ECase e alts)              p = 
  parenIf (p>0) (iIndent (iConcat [iStr tkCase, iStr " ", pprExpr e 0, iStr " ", iStr tkOf,
                                   iStr " ", iStr tkOCB, iStr " ", iNewline,
                                   iStr "  ", iIndent (pprAlts alts), iNewline,
                                   iStr tkCCB ]))
pprExpr (ELam args expr)            p =
   parenIf (p>0) (iConcat [iStr tkLambda, pprArgs args, iStr " ", iStr tkDot, iStr " ",
                           pprExpr expr 0])
pprExpr (ELet isRec defns e)        p =
   parenIf (p>0) (iIndent (iConcat [iStr keyWord, iStr " ", iStr tkOCB, iStr " ",
                                    iIndent (pprDefns defns), iNewline,
                                    iStr tkCCB, iStr " ", iStr tkIn,
                                    iStr " ", pprExpr e 0]))
                       where keyWord | isRec     = tkLetrec
                                     | otherwise = tkLet

                     
pprAExpr :: Show a => Expr a -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e 0
	   | otherwise      = iConcat [iStr tkOB, pprExpr e 0, iStr tkCB]


pprAlts :: Show a => [Alter a] -> Iseq 
pprAlts alts = iInterleave sep (map pprAlter alts)
  where sep = iConcat [iStr tkSemicolon, iNewline]


pprAlter :: Show a => Alter a -> Iseq   
pprAlter (MkAlter (tag,args,expr)) = 
   iConcat [iStr tkLT, iNum tag, iStr tkGT, iStr " ",
            pprArgs args, iStr " ", iStr tkArrow, iStr " ", pprExpr expr 0]
  where sep = iConcat [iStr tkSemicolon, iNewline]


pprDefns :: Show a => [Defn a] -> Iseq 
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [iStr tkSemicolon, iNewline]


pprDefn :: Show a => Defn a -> Iseq 
pprDefn (MkDefn (name, expr)) =
   iConcat [iStr (myShow name), iStr " ", iStr tkEq, iStr " ",
            iIndent (pprExpr expr 0)]


pprProgram :: Show a => Program a -> Iseq
pprProgram (MkProgram [])   = iNil
pprProgram (MkProgram defs) = iInterleave sep (map pprScDefn defs)
  where sep = iConcat [iStr tkSemicolon, iNewline, iNewline ]


pprScDefn :: Show a => ScDefn a -> Iseq 
pprScDefn (MkScDefn (name, args, expr)) =
   iConcat [iStr name, iStr " ", pprArgs args, iStr " ", iStr tkEq,
            iStr "  ", pprExpr expr 0]


pprArgs :: Show a => [a] -> Iseq 
pprArgs args = iInterleave (iStr " ") (map (iStr.myShow) args)


instance Show a => PPrintable (ScDefn a) where
  toIseq sc    = pprScDefn sc

instance Show a => PPrintable (Program a) where
  toIseq prog  = pprProgram prog

instance Show a => PPrintable (Defn a) where
  toIseq defn  = pprDefn defn

instance Show a => PPrintable (Alter a) where
  toIseq alter = pprAlter alter

instance Show a => PPrintable (Expr a) where
  toIseq expr  = pprExpr expr 0





{-
pprExpr :: Show a => Expr a -> Iseq
pprExpr (EVar v)             = iStr v
pprExpr (ENum n)             = iStr (show n)
pprExpr (EConstr tag arity)  = iConcat [iStr tkPack, iStr " ", iStr tkOCB, iStr " ", iNum tag, iStr tkComma, iNum arity, iStr tkCCB]
pprExpr (EAp e1 e2)          = iConcat [pprExpr e1, iStr " ", pprAExpr e2 ]
pprExpr (ECase e alts)       = iIndent (iConcat [ iStr tkCase, iStr " ", pprExpr e, iStr " ", iStr tkOf, iStr " ", iStr tkOCB, iStr " ", iNewline,
                                                  iStr "  ", iIndent (pprAlts alts), iNewline,
                                                  iStr tkCCB ])
pprExpr (ELam args expr)     = iConcat [iStr tkLambda, pprArgs args, iStr " ", iStr tkDot, iStr " ", pprExpr expr]
pprExpr (ELet isRec defns e) = iIndent (iConcat [iStr word, iStr " ", iStr tkOCB, iStr " ", iIndent (pprDefns defns), iNewline,
                                                 iStr tkCCB, iStr " ", iStr tkIn, iStr " ", pprExpr e])
 where word | isRec     = tkLetrec
            | otherwise = tkLet

-}
