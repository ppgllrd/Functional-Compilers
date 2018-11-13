-------------------------------------------------------
-- G-Machine vstack
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------

module GMVStack (
         GMVStack,
         vstackEmpty,
         vstackPush,
         vstackPop,
         vstackSelect,
         vstackLength,
         vstackToList,
         vstackFromList
       ) where

import LibStack

type GMVStack = Stack Int

vstackEmpty :: GMVStack
vstackEmpty = stackEmpty

vstackPush :: GMVStack -> Int -> GMVStack
vstackPush = stackPush

vstackPop :: GMVStack -> Maybe (GMVStack, Int)
vstackPop = stackPop

vstackLength :: GMVStack -> Int
vstackLength = stackLength

vstackSelect :: GMVStack -> Int -> Maybe Int
vstackSelect = stackSelect

vstackToList :: GMVStack -> [Int]
vstackToList = stackToList

vstackFromList :: [Int] -> GMVStack
vstackFromList = stackFromList
