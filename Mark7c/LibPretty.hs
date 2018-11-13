-------------------------------------------------
-- José E. Gallardo, July 1997
--
-- A library for Prettty Printing as described in
-- 
-- "Implementing Functional languages"
--
-------------------------------------------------

module LibPretty (
         PPrintable(toIseq), pprint,
         Iseq,
         iNil, iStr, iAppend, iNewline, iIndent, iDisplay,
         iConcat, iInterleave, 
         iInt, iFWInt, iLayn,
         flatten,
       ) where

infixr 8 `iAppend`

data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline

iNil :: Iseq                  		-- The empty iseq
iNil = INil

iStr :: String -> Iseq        		-- Turn a string into an iseq
iStr str = IStr str

iAppend  :: Iseq -> Iseq -> Iseq  	-- Append two iseqs
iAppend seq1 seq2 = IAppend seq1 seq2

iNewline :: Iseq                  	-- New line with indentation
iNewline = INewline

iIndent  :: Iseq -> Iseq          	-- Indent an iseq
iIndent seq = IIndent seq


iDisplay :: Iseq -> String        	-- Turn an iseq into a string
iDisplay seq = flatten 0 [(seq,0)]

iConcat :: [Iseq] -> Iseq
iConcat []     = INil
iConcat (s:ss) = IAppend s (iConcat ss)

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave sep []     = INil
iInterleave sep [s]    = s
iInterleave sep (s:ss) = IAppend s (IAppend sep (iInterleave sep ss))


flatten :: Int                       -- Current column; 0 for first column
            -> [(Iseq, Int)]         -- Work list
            -> String                -- Result

flatten col []                             = ""
flatten col ((INil,          indent):seqs) = flatten col seqs
flatten col ((IStr str,      indent):seqs) = str ++ flatten (col+length str) seqs
flatten col ((IAppend s1 s2, indent):seqs) = flatten col ((s1,indent):(s2,indent):seqs)
flatten col ((IIndent seq,   indent):seqs) = flatten col ((seq,col):seqs) 
flatten col ((INewline,      indent):seqs) = "\n" ++ (spaces indent) ++ (flatten indent seqs)


iInt :: Int -> Iseq
iInt n = iStr (show n)

iFWInt :: Int -> Int -> Iseq
iFWInt width n
  = iStr (spaces (width - length digits) ++ digits)
    where
    digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
             where
             lay_item (n, seq)
               = iConcat [ iFWInt 4 n, iStr ") ", iIndent seq, iNewline ]


class PPrintable a where
  toIseq :: a -> Iseq

  
pprint :: PPrintable a => a -> String
pprint = iDisplay . toIseq



-- Function private to module
spaces :: Int -> String
spaces n | n <= 0    = ""
         | otherwise = ' ' : spaces (n-1)



