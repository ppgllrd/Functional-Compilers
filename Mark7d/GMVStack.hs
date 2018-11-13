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
import GlobalDefs

type GMVStack = Stack BasicValue

vstackEmpty :: GMVStack
vstackEmpty = stackEmpty

vstackPush :: GMVStack -> BasicValue -> GMVStack
vstackPush = stackPush

vstackPop :: GMVStack -> Maybe (GMVStack, BasicValue)
vstackPop = stackPop

vstackLength :: GMVStack -> Int
vstackLength = stackLength

vstackSelect :: GMVStack -> Int -> Maybe BasicValue
vstackSelect = stackSelect

vstackToList :: GMVStack -> [BasicValue]
vstackToList = stackToList

vstackFromList :: [BasicValue] -> GMVStack
vstackFromList = stackFromList
