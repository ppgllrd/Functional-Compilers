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

type GMSStack = Stack Address

sstackEmpty :: GMSStack
sstackEmpty = stackEmpty

sstackPush :: GMSStack -> Address -> GMSStack
sstackPush = stackPush

sstackPop :: GMSStack -> Maybe (GMSStack, Address)
sstackPop = stackPop

sstackLength :: GMSStack -> Address
sstackLength = stackLength

sstackSelect :: GMSStack -> Address -> Maybe Address
sstackSelect = stackSelect

sstackToList :: GMSStack -> [Address]
sstackToList = stackToList

sstackFromList :: [Address] -> GMSStack
sstackFromList = stackFromList
