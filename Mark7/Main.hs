-------------------------------------------------------
-- G-Machine main entry
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------


module Main where

import CoreParser
import Compiler
import GMachine
import LibSIO (runSIO)


doTrace :: Bool
doTrace = False

main :: IO ()
main = do str <- readFile "Core5.txt"
	  let (parsed, st) = parse str
          let initialSt = compile parsed
          (finalSt, ()) <- runSIO (if doTrace then trace else eval) initialSt
          return ()








