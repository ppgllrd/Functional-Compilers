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
import CoreLang
import Compiler
import GMachine
import LibPretty
import LibSIO (runSIO)
import CCompiler

doTrace :: Bool
doTrace = True

main1 :: IO ()
main1 = do strPrelude <- readFile "Prelude.core"
	   let (parsedPrelude, stPrelude) = parse strPrelude
           str <- readFile "core6.txt"
	   let (parsed, st) = parse str
--	   putStr (pprint parsed)
           let initialSt = compile parsed parsedPrelude
           (finalSt, ()) <- runSIO (if doTrace then trace else eval) initialSt
           return ()


main :: IO ()
main = do 
 strPrelude <- readFile "Prelude.core"
 let (parsedPrelude, stPrelude) = parse strPrelude
 putStr "Nombre del fichero core a compilar: "
 fileName <-  getLine
 str <- readFile fileName
 let (parsed, st) = parse str
-- putStrLn $ iDisplay (pprProgram parsed)
 let initialSt = compile parsed parsedPrelude
 writeFile "..\\c\\out.c" (ccompile initialSt)
 putStrLn "Compilation is in ..\\c\\out.c" 
 return ()







