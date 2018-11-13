-------------------------------------------------------
-- G-Machine code
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------

module GMCode (
 	 Instruction(..),
         GMCode,
         codeFromList, codeToList,
         codeIsNull,
         iShortCode
       ) where

import GlobalDefs
import LibPretty
import Monad

data Instruction = Unwind
		 | Pushglobal Name
		 | Pushint Int
		 | Pushchar Char
		 | Push Int
		 | Mkap
		 | Update Int
		 | Pop Int
                 | Slide Int
                 | Alloc Int
                 | Eval
                 | IAdd | ISub  | IMul | IDiv | INegate
                 | IEQT | INEQT | ILT  | ILE  | IGT  | IGE
                 | COrd | CChr
                 | Cond GMCode GMCode
                 | Pack Int Int
                 | Casejump [(Int, GMCode)]
                 | Split Int
                 | Print
                 | PrintStr String
                 | Mkbool | Mkint | Mkchar
                 | Get
                 | Pushbasic BasicValue
                 | Return
		deriving Eq

showInstruction :: Instruction -> Iseq
showInstruction (Unwind)          = iStr "Unwind"
showInstruction (Pushglobal name) = iConcat [iStr "Pushglobal ", iStr name]
showInstruction (Pushint int)	  = iConcat [iStr "Pushint ", iInt int]
showInstruction (Pushchar char)	  = iConcat [iStr "Pushchar ", iStr (show char)]
showInstruction (Push int)	  = iConcat [iStr "Push ", iInt int]
showInstruction (Mkap)		  = iStr "Mkap"
showInstruction (Update int)	  = iConcat [iStr "Update ", iInt int]
showInstruction (Pop int)	  = iConcat [iStr "Pop ", iInt int]
showInstruction (Slide int)	  = iConcat [iStr "Slide ", iInt int]
showInstruction (Alloc int)	  = iConcat [iStr "Alloc ", iInt int]
showInstruction (Eval)	          = iStr "Eval"
showInstruction (IAdd)	          = iStr "IAdd"
showInstruction (ISub)	          = iStr "ISub"
showInstruction (IMul)	          = iStr "IMul"
showInstruction (IDiv)	          = iStr "IDiv"
showInstruction (INegate)	  = iStr "INegate"
showInstruction (IEQT)	          = iStr "IEQT"
showInstruction (INEQT)	          = iStr "INEQT"
showInstruction (ILT)	          = iStr "ILT"
showInstruction (ILE)	          = iStr "ILE"
showInstruction (IGT)	          = iStr "IGT"
showInstruction (IGE)	          = iStr "IGE"
showInstruction (COrd)	          = iStr "COrd"
showInstruction (CChr)	          = iStr "CChr"
showInstruction (Cond c1 c2)	  = iConcat [iStr "Cond [", iIndent (iConcat [iStr "2: ", iCode c1, iNewline,
                                                                              iStr "1: ", iCode c2
                                                                     ]), iNewline,
                                             iStr "]"
                                    ]
showInstruction (Pack n1 n2)      = iConcat [iStr "Pack ", iInt n1, iStr " ", iInt n2]
showInstruction (Casejump xs)     = iConcat [iStr "Casejump ",  iIndent (iConcat (map showCase xs))]
				      where showCase (n, code) = iConcat [iInt n, iStr ": ", iCode code,iNewline]
showInstruction (Split n)         = iConcat [iStr "Split ", iInt n]
showInstruction (Print)	          = iStr "Print"
showInstruction (PrintStr str)    = iConcat [iStr "PrintStr ", iStr str]
showInstruction (Mkbool)          = iStr "Mkbool"
showInstruction (Mkint)           = iStr "Mkint"
showInstruction (Mkchar)          = iStr "Mkchar"
showInstruction (Get)             = iStr "Get"
showInstruction (Pushbasic v)     = iConcat [iStr "Pushbasic ", iStr (show v)]
showInstruction (Return)          = iStr "Return"



iCode :: GMCode -> Iseq    
iCode (MkGMCode code)
    = iConcat [iStr "{", iIndent (iInterleave (iNewline) codes), iStr"}"]
    where codes    = map showInstruction code
                                         
iShortCode :: Int -> GMCode -> Iseq    
iShortCode int (MkGMCode code)
    = iConcat [iStr "{", iIndent (iInterleave (iNewline) dotcodes), iStr"}"]
    where codes    = map showInstruction (take int code)
          dotcodes | length code > int = codes ++ [iStr "..."]
                   | otherwise         = codes


instance PPrintable Instruction where
  -- toIseq :: Instruction -> Iseq
  toIseq = showInstruction


newtype GMCode' a = MkGMCode  [a] deriving Eq

type GMCode       = GMCode' Instruction

codeFromList :: [Instruction] -> GMCode
codeFromList xs = MkGMCode xs


codeToList :: GMCode -> [Instruction]
codeToList (MkGMCode xs) = xs

codeIsNull :: GMCode -> Bool
codeIsNull (MkGMCode xs) = null xs

instance MonadPlus GMCode' where
  -- mzero :: GMCode' a
  mzero = MkGMCode []

  -- mplus :: GMCode' a -> GMCode' a -> GMCode' a
  (MkGMCode c1) `mplus` (MkGMCode c2) = MkGMCode (c1++c2)

instance Monad GMCode' where
  -- return :: a -> GMCode' a
  return x = MkGMCode [x]

  -- (>>=) :: GMCode' a -> (a -> GMCode' b) -> GMCode' b
  MkGMCode []      >>= f  = MkGMCode []
  MkGMCode (x:xs)  >>= f  = let MkGMCode fx  = f x 
                                MkGMCode fxs = MkGMCode xs >>= f
                            in MkGMCode (fx++fxs)
