-------------------------------------------------------
-- A Parser monad
--
-- José E. Gallardo, December 1997
-------------------------------------------------------

module LibParser (
	 Parser,
         readState, writeState,  -- Access to user state
	 fatalError,		 -- Raise a parse error
	 runParser,		 -- Runs a parser
	 readAChar,		 -- Read next character
	 lookupAChar,
	 pSatisfy,
	 many1, many,
	 sepby, sepby1,
	 chainl,chainl1,
	 chainr,chainr1,
	 ops,bracket,
	 pChar, pDigit, pLower, pUpper, pLetter, pAlphanum, 
	 pString, pNat, pInt, pSpaces
       ) where


import Char

-- Parsers analise strings

type Input        = String


-------------------------------------------------------
-- Positions in text
-------------------------------------------------------

type Row      = Int
type Col      = Int
data Position = MkPos { 
		  row :: Row,
                  col :: Col 
                }

instance Eq Position where
  -- (==) :: Position -> Position -> Bool
  p1 == p2  = row p1 == row p2  && col p1 == col p2

instance Ord Position where
  -- (<=) :: Position -> Position -> Bool
  p1 <= p2  = p1 == p2 || row p1 < row p2 || (row p1 == row p2 && col p1 < col p2) 


instance Show Position where
  -- showsPrec :: Int -> Postion -> ShowS
  showsPrec p pos = showString "Line:"   .
                    shows (row pos)      .
                    showString "  Col:" .
                    shows (col pos)


-------------------------------------------------------
-- Errors
-------------------------------------------------------

type ErrorMsg   = String
data Error      = MkError { 
		    errorMsg   :: ErrorMsg,
		    errorPos   :: Position,
                    errorInput :: Input
		  }
type FatalError = Error


instance Eq Error where
  -- (==) :: Error -> Error -> Bool
  e1 == e2  = errorPos e1 == errorPos e2

instance Ord Error where
  -- (<) :: Error -> Error -> Bool
  e1 <= e2   =  errorPos e1 <= errorPos e2


-------------------------------------------------------
-- The Monad State
-------------------------------------------------------

data State userSt = MkState { 
                      input     :: Input,
		      stPos     :: Position,
                      userState :: userSt
                    }


-------------------------------------------------------
-- The Monad
-------------------------------------------------------

data Either3 a b c = Left3 a | Middle3 b | Right3 c

newtype Parser s a = MkPM (State s -> Either3 FatalError Error (State s,a))

instance Monad (Parser s) where
  -- return :: a -> Parser s a
  return x         = MkPM (\s -> Right3 (s,x))

  -- (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  MkPM st  >>= f  = MkPM (\s -> case st s of
 		      Left3 err        -> Left3 err
 		      Middle3 fatalErr -> Middle3 fatalErr
		      Right3 (s',x)    -> let (MkPM st') = f x
				          in st' s'
                    )


fatalError :: ErrorMsg -> Parser s a
fatalError str = do r <- readRow
                    c <- readCol
                    i <- readInput
                    raiseFatal MkError { errorMsg   = str,
               			         errorPos   = MkPos {row=r, col=c},
                                         errorInput = i}
  where raiseFatal :: Error -> Parser s a
	raiseFatal fatalErr = MkPM (\s -> Middle3 fatalErr)


instance MonadZero (Parser s) where
  -- zero :: Parser s a
  zero = do r <- readRow
            c <- readCol
            i <- readInput
            raiseException MkError { errorMsg   = "Parse error",
            			     errorPos   = MkPos {row=r, col=c},
                                     errorInput = i   }
	   where raiseException :: Error -> Parser s a
		 raiseException err  = MkPM (\s -> Left3 err)


instance MonadPlus (Parser s) where
  -- (++) :: Parser s a -> Parser s a -> Parser s a 
  MkPM st1  ++  MkPM st2  = MkPM (\s -> case st1 s of
				  	  Right3 (s', x)   -> Right3 (s', x)
					  Middle3 fatalErr -> Middle3 fatalErr
				  	  Left3 err        -> case st2 s of
							       Right3 (s'', x')  -> Right3 (s'', x')
							       Middle3 fatalErr' -> Middle3 fatalErr'
							       Left3 err'        -> Left3 (max err err')
			    )

runSTE :: Parser s a -> (State s) -> Either3 FatalError Error (State s,a) 
runSTE (MkPM st) s0 = st s0


-------------------------------------------------------
-- Access to the state
-------------------------------------------------------

updateST :: (State s -> State s) -> Parser s (State s)
updateST f = MkPM (\s -> Right3 (f s, s))

readST :: Parser s (State s)
readST = updateST id

writeST :: (State s) -> Parser s ()
writeST s = do updateST (\_ -> s)
               return ()

-- get current row
readRow :: Parser s Row
readRow = do st <- readST
             let p = stPos st
             let r = row p
             return r

-- get current column
readCol :: Parser s Row
readCol = do st <- readST
             let p = stPos st
             let c = col p
             return c

-- get current user state
readState :: Parser s s
readState = do st <- readST
               let uSt = userState st
	       return uSt


-- get current Input
readInput :: Parser s Input
readInput = do st <- readST
               let i = input st
               return i

-- set a new row
writeRow :: Row -> Parser s ()
writeRow row' = do st <- readST
                   writeST MkState { input     = input st,
                                     stPos     = MkPos {row=row', col=col(stPos st)},
                                     userState = userState st}

-- set a new column
writeCol :: Col -> Parser s ()
writeCol col' = do st <- readST
                   writeST MkState { input     = input st,
                                     stPos     = MkPos {row=row(stPos st), col=col'},
                                     userState = userState st}

-- set a new state
writeState :: s -> Parser s ()
writeState uSt' = do st <- readST
                     writeST MkState { input     = input st,
                                       stPos     = stPos st,
                                       userState = uSt'}


-- set a new input
writeInput :: Input -> Parser s ()
writeInput input' = do st <- readST
                       writeST MkState { input     = input',
                                         stPos     = stPos st,
                                         userState = userState st}


force             :: Parser s a -> Parser s a
force (MkPM st)  = MkPM (\s -> let x = st s        
			       in case x of 
                                Right3 _         -> Right3 (fst (f x) , snd (f x))
				Middle3 fatalErr -> Middle3 fatalErr
                                )
		     where f (Right3 x) = x


-------------------------------------------------------
-- Parsers
-------------------------------------------------------

readAChar 	  :: Parser s Char
readAChar 	   = do (c:cs) <- readInput
               		if c == '\n' then do row <- readRow
				 	     writeRow (row+1)
				    	     writeCol 1
			    	     else do col <- readCol
				    	     writeCol (col+1)
		        writeInput cs
		        return c



lookupAChar	  :: Parser s Char
lookupAChar	   = do (c:cs) <- readInput
			return c
			

pSatisfy 	  :: (Char -> Bool) -> Parser s Char
pSatisfy p 	   = do c <- readAChar
               		guard (p c)
               		return c


many1 		  :: Parser s a -> Parser s [a]
many1 p 	   = do x  <- p 
	                xs <- do {many1 p ++ return []}
           	        return (x:xs)


many 	  	  :: Parser s a -> Parser s [a]
many p 		   = force (many1 p ++ return [])


sepby             :: Parser s a -> Parser s b -> Parser s [a]
p `sepby` sep      = (p `sepby1` sep) ++ return []


sepby1            :: Parser s a -> Parser s b -> Parser s [a]
p `sepby1` sep     = do {x <- p; xs <- many (do {sep; p}); return (x:xs)}


chainl            :: Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainl p op v      = (p `chainl1` op) ++ return v


chainl1           :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
p `chainl1` op     = do {x <- p; rest x}
                     where
                        rest x = do {f <- op; y <- p; rest (f x y)}
                                 ++ return x


chainr            :: Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainr p op v      = (p `chainr1` op) ++ return v


chainr1           :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
p `chainr1` op     = do {x <- p; rest x}
                     where
                        rest x = do {f <- op; y <- p `chainr1` op; return (f x y)} 
                                 ++ return x
                                 

ops               :: [(Parser s a, b)] -> Parser s b
ops xs             = foldr1 (++) [do {p; return op} | (p,op) <- xs]


bracket           :: Parser s a -> Parser s b -> Parser s c -> Parser s b
bracket open p close = do {open; x <- p; close; return x}

-------------------------------------------------------
-- Useful parsers 
-------------------------------------------------------

pChar              :: Char -> Parser s Char
pChar x             = pSatisfy (\y -> x == y)

pDigit             :: Parser s Char
pDigit              = pSatisfy isDigit

pLower             :: Parser s Char
pLower              = pSatisfy isLower

pUpper             :: Parser s Char
pUpper              = pSatisfy isUpper

pLetter            :: Parser s Char
pLetter             = pSatisfy isAlpha

pAlphanum          :: Parser s Char
pAlphanum           = pSatisfy isAlphanum

pString            :: String -> Parser s String
pString ""          = return ""
pString (x:xs)      = do {pChar x; pString xs; return (x:xs)}

pNat               :: Parser s Int
pNat                = do {x <- pDigit; return (digitToInt x)} `chainl1` return op
                      where
                        m `op` n = 10*m + n

pInt               :: Parser s Int
pInt                = do {pChar '-'; n <- pNat; return (-n)} ++ pNat

pSpaces            :: Parser s ()
pSpaces             = do {many1 (pSatisfy isSpace); return ()}


-------------------------------------------------------


runParser :: Parser s a -> Input -> s -> (a,s)
runParser p str st0 = 
     case runSTE p initST of 
	Left3 err         -> showError (errorMsg err) (errorPos err) (errorInput err)
	Middle3 fatalErr  -> error (errorMsg fatalErr++" at "++show (errorPos fatalErr))
	Right3 (st, res)  -> if null (input st) then (res, userState st)
				                else let pos    = stPos st
							 newPos = MkPos {row=row pos, col=col pos+1}
                                                         in showError "Parse error at " newPos (input st)
      where initST = MkState { input     = str,
                               stPos     = MkPos {row=1, col=1},
	 	               userState = st0 }
            showError errMsg pos input = error (errMsg ++ " at " ++ show pos ++ format input pos)
            format [] pos = " at end of file"
            format xs pos = "\nUnexpected\n"++(if col pos>=5 then spaces (col pos-5)++"..." else "...\n"++spaces (col pos-2)) ++take 50 xs++"..."
            spaces n  | n > 0      = take n (repeat ' ')
                      | otherwise  = ""

------------------------------------------------------------------------------

