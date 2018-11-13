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

data Instruction = Unwind
		 | Pushglobal Name
		 | Pushint Int
		 | Push Int
		 | Mkap
		 | Update Int
		 | Pop Int
                 | Slide Int
                 | Alloc Int
                 | Eval
                 | IAdd | ISub | IMul | IDiv | INegate
                 | IEQT  | INEQT  | ILT  | ILE  | IGT  | IGE
                 | Cond GMCode GMCode
                 | Pack Int Int
                 | Casejump [(Int, GMCode)]
                 | Split Int
                 | Print
                 | PrintStr String
		deriving Eq

showInstruction :: Instruction -> Iseq
showInstruction (Unwind)          = iStr "Unwind"
showInstruction (Pushglobal name) = iConcat [iStr "Pushglobal ", iStr name]
showInstruction (Pushint int)	  = iConcat [iStr "Pushint ", iNum int]
showInstruction (Push int)	  = iConcat [iStr "Push ", iNum int]
showInstruction (Mkap)		  = iStr "Mkap"
showInstruction (Update int)	  = iConcat [iStr "Update ", iNum int]
showInstruction (Pop int)	  = iConcat [iStr "Pop ", iNum int]
showInstruction (Slide int)	  = iConcat [iStr "Slide ", iNum int]
showInstruction (Alloc int)	  = iConcat [iStr "Alloc ", iNum int]
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
showInstruction (Cond c1 c2)	  = iConcat [iStr "Cond [2: ", iShortCode 3 c1,
                                             iStr ",     1: ", iShortCode 3 c2, 
                                             iStr "]"
                                    ]
showInstruction (Pack n1 n2)      = iConcat [iStr "Pack ", iNum n1, iStr " ", iNum n2]
showInstruction (Casejump xs)     = iConcat [iStr "Casejump ",  iIndent (iConcat (map showCase xs))]
				      where showCase (n, code) = iConcat [iNum n, iStr ": ", iCode code]
showInstruction (Split n)         = iConcat [iStr "Split ", iNum n]
showInstruction (Print)	          = iStr "Print"
showInstruction (PrintStr str)    = iConcat [iStr "Print ", iStr str]



iCode :: GMCode -> Iseq    
iCode (MkGMCode code)
    = iConcat [iStr "{", iInterleave (iStr "; ") codes, iStr"}" ]
    where codes    = map showInstruction code
                                         
iShortCode :: Int -> GMCode -> Iseq    
iShortCode number (MkGMCode code)
    = iConcat [iStr "{", iInterleave (iStr "; ") dotcodes, iStr"}" ]
    where codes    = map showInstruction (take number code)
          dotcodes | length code > number = codes ++ [iStr "..."]
                   | otherwise            = codes


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

instance MonadZero GMCode' where
  -- zero :: GMCode' a
  zero = MkGMCode []

instance MonadPlus GMCode' where
  -- (++) :: GMCode' a -> GMCode' a -> GMCode' a
  (MkGMCode c1) ++ (MkGMCode c2) = MkGMCode (c1++c2)

instance Monad GMCode' where
  -- return :: a -> GMCode' a
  return x = MkGMCode [x]

  -- (>>=) :: GMCode' a -> (a -> GMCode' b) -> GMCode' b
  MkGMCode []      >>= f  = MkGMCode []
  MkGMCode (x:xs)  >>= f  = let MkGMCode fx  = f x 
                                MkGMCode fxs = MkGMCode xs >>= f
                            in MkGMCode (fx++fxs)
