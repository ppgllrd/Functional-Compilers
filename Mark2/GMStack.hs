-------------------------------------------------------
-- G-Machine global names
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------

module GMStack (
         GMStack,
         stackEmpty,
         stackPush,
         stackPop,
         stackSelect,
         stackLength,
         stackToList
       ) where

import GMHeap

newtype GMStack = MkGMStack [Addr]

stackEmpty :: GMStack 
stackEmpty = MkGMStack []

stackPush :: GMStack -> Addr -> GMStack
stackPush (MkGMStack stack) a = MkGMStack (a:stack)

stackPop :: GMStack -> Maybe (GMStack, Addr)
stackPop (MkGMStack [])     = Nothing
stackPop (MkGMStack (a:as)) = Just ((MkGMStack as), a)

stackLength :: GMStack -> Int
stackLength (MkGMStack stack) = length stack

stackSelect :: GMStack -> Int -> Maybe Addr
stackSelect (MkGMStack [])     _     = Nothing
stackSelect (MkGMStack (a:as)) 0     = Just a
stackSelect (MkGMStack (a:as)) (n+1) = stackSelect (MkGMStack as) n

stackToList :: GMStack -> [Addr]
stackToList (MkGMStack as) = as

