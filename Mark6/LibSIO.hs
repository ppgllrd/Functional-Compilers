--------------------------------------------------------
-- A monad that supports IO, state and error handling
--
-- José E. Gallardo, December 1997
--------------------------------------------------------

module LibSIO (
        SIO,
        liftSIO, runSIO,
        fail, catch,
        putStr, print,
        readST, writeST 
       ) where


-- Redefined functions from Prelude
import Prelude hiding (catch, fail, putStr, print)
import qualified Prelude (catch, fail, putStr, print)


data SIO s a = MkSIO (s ->  IO (s, a))

instance Monad (SIO s) where
  -- return :: a -> SIO s a
  return x = MkSIO (\s -> return (s, x))

  -- (>>=) :: SIO s a -> (a -> SIO s b) -> SIO s b
  MkSIO m  >>= f  = MkSIO (\s -> m s >>= \(s', x) ->
                                 let MkSIO st' = f x
			         in st' s')  

-- Lifting an IO operation
liftSIO :: IO a -> SIO s a 
liftSIO io = MkSIO (\s -> io >>= \x -> return (s, x))


-- Errors using IO fail
fail :: IOError -> SIO s a
fail ioErr = liftSIO (Prelude.fail ioErr)

catch :: SIO s a -> (IOError -> SIO s a) -> SIO s a
(MkSIO st) `catch` f = MkSIO (\s -> do (s', x) <- st s
                                       return (s', x)
                                    `Prelude.catch` (\err -> let MkSIO st' = f err
                                                             in st' s))
                   

instance MonadZero (SIO s) where
  -- zero :: SIO s a
  zero = fail (userError "SIO s:: zero")
  
instance MonadPlus (SIO s) where
  -- (++) :: SIO s a -> SIO s a -> SIO s a
  m1 ++ m2 = m1 `catch` (\_ -> m2)  

-- Running an SIO computation from an initial state
runSIO :: SIO s a -> s -> IO (s,a)
runSIO (MkSIO st) s0 = st s0


-- Handling of the monad state
updateST :: (s->s) -> SIO s s
updateST f = MkSIO (\s -> return (f s, s))

readST :: SIO s s
readST = updateST id

writeST :: s -> SIO s ()
writeST s = do updateST (const s)
               return ()


-- Aditional liftings
putStr :: String -> SIO s ()
putStr str = liftSIO (Prelude.putStr str)

print :: Show a => a -> SIO s ()
print x = liftSIO (Prelude.print x)


 