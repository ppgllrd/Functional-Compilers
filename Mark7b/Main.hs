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
import LibPretty
import LibSIO (runSIO)


doTrace :: Bool
doTrace = False

main :: IO ()
main = do strPrelude <- readFile "Prelude.core"
	  let (parsedPrelude, stPrelude) = parse strPrelude
          str <- readFile "Core6.txt"
	  let (parsed, st) = parse str
--	  putStr (pprint parsed)
          let initialSt = compile parsed parsedPrelude
          (finalSt, ()) <- runSIO (if doTrace then trace else eval) initialSt
          return ()








