-------------------------------------------------------
-- G-Machine sstack
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------

module GMSStack (
         GMSStack,
         sstackEmpty,
         sstackPush,
         sstackPop,
         sstackSelect,
         sstackLength,
         sstackToList,
         sstackFromList
       ) where

import LibStack
import GMHeap

type GMSStack = Stack Addr

sstackEmpty :: GMSStack
sstackEmpty = stackEmpty

sstackPush :: GMSStack -> Addr -> GMSStack
sstackPush = stackPush

sstackPop :: GMSStack -> Maybe (GMSStack, Addr)
sstackPop = stackPop

sstackLength :: GMSStack -> Addr
sstackLength = stackLength

sstackSelect :: GMSStack -> Addr -> Maybe Addr
sstackSelect = stackSelect

sstackToList :: GMSStack -> [Addr]
sstackToList = stackToList

sstackFromList :: [Addr] -> GMSStack
sstackFromList = stackFromList
