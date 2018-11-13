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
         codeIsNull
       ) where

import GlobalDefs
import LibPretty

data Instruction = Unwind
		 | Pushglobal Name
		 | Pushint Int
		 | Push Int
		 | Mkap
		 | Slide Int
		deriving Eq

showInstruction :: Instruction -> Iseq
showInstruction Unwind		  = iStr "Unwind"
showInstruction (Pushglobal name) = iConcat [iStr "Pushglobal ", iStr name]
showInstruction (Pushint int)	  = iConcat [iStr "Pushint ", iNum int]
showInstruction (Push int)	  = iConcat [iStr "Push ", iNum int]
showInstruction Mkap		  = iStr "Mkap"
showInstruction (Slide int)	  = iConcat [iStr "Slide ", iNum int]

instance PPrintable Instruction where
  -- toIseq :: Instruction -> Iseq
  toIseq = showInstruction


newtype GMCode' a = MkGMCode  [a]

type GMCode       = GMCode' Instruction



codeFromList :: [Instruction] -> GMCode
codeFromList xs = MkGMCode xs


codeToList :: GMCode -> [Instruction]
codeToList (MkGMCode xs) = xs

codeIsNull :: GMCode -> Bool
codeIsNull (MkGMCode xs) = null xs

-- istackEmpty
-- setCode

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
