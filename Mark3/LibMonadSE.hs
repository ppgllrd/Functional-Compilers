-------------------------------------------------------
-- A Monad with error and state
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------


module LibMonadSE (
         MSE,
         ErrorMsg,
         raiseError,
         runMSE,
         writeST,
         readST
       ) where


-------------------------------------------------------
-- The monad
-------------------------------------------------------


type ErrorMsg = String

newtype MSE s a = MkMSE (s -> Either ErrorMsg (s,a))

instance Monad (MSE s) where
  -- return :: a -> MSE s a
  return x = MkMSE (\s -> Right (s,x))

  -- (>>=) :: MSE s a -> (a -> MSE s b) -> MSE s b
  MkMSE st >>= f  = MkMSE (\s -> case st s of
                                   Left err     -> Left err
                                   Right (s',x) -> let MkMSE st' = f x
                                                   in st' s')


runMSE :: MSE s a -> s -> (s,a)
runMSE (MkMSE a) s = case a s of
                                Left err    -> error err
                                Right (s,x) -> (s,x)



raiseError :: ErrorMsg -> MSE s a
raiseError err = MkMSE (\s -> Left err)


instance MonadZero (MSE s) where
  -- zero :: MSE s a
  zero = raiseError "MSE zero"

instance MonadPlus (MSE s) where
  -- (++) :: MSE s a -> MSE s a -> MSE s a
  (MkMSE st1) ++ (MkMSE st2) = MkMSE (\s -> case st1 s of
                                              Right (s',x) -> Right (s',x)
                                              Left err     -> st2 s)

updateST :: (s -> s) -> MSE s s
updateST f = MkMSE (\s -> Right (f s, s))

readST :: MSE s s
readST = updateST id

writeST :: s -> MSE s ()
writeST s = do updateST (\_ -> s)
               return ()


