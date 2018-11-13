-------------------------------------------------------
-- G-Machine heap
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------


module GMHeap (
         GMHeap,
         Node(..),
         Address,
         heapInitial, heapAlloc, heapUpdate, heapFree, heapLookup,
         heapSize, heapNull, heapIsNull,
         heapAddressesses,
         showAddress
       ) where
 

import GMCode
import LibAssoc
import List

data Heap a = MkHeap Int [Int] (Assoc Int a)

newtype GMHeap  = MkGMHeap  (Heap Node)

data Node = NNum Int		-- Numbers
	  | NAp Address Address	-- Applications
	  | NGlobal Int GMCode  -- Global definitions
          | NInd Address           -- Indirections
          | NConstr Int [Address]  -- Constructors
          

type Address   = Int

heapInitial :: GMHeap 
heapInitial = MkGMHeap (MkHeap 0 [1..] aEmpty)


heapAlloc :: GMHeap -> Node -> (GMHeap, Address)
heapAlloc (MkGMHeap (MkHeap size (next:free) cts)) n = (MkGMHeap (MkHeap (size+1) free (aUnion (aSingleton next n) cts)), next)


heapUpdate :: GMHeap -> Address -> Node -> GMHeap
heapUpdate (MkGMHeap (MkHeap size free cts)) a n = MkGMHeap (MkHeap size free (aUnion newA (aSubtraction cts oldA)))
  where newA = aSingleton a n
        oldA = let (Just oldN) = aLookup cts a
               in aSingleton a oldN


heapFree :: GMHeap -> Address -> GMHeap
heapFree (MkGMHeap (MkHeap size free cts)) a = MkGMHeap (MkHeap (size-1) (a:free) (aSubtraction cts oldA))
  where oldA = let (Just oldN) = aLookup cts a
               in aSingleton a oldN


heapLookup :: GMHeap -> Address -> Maybe Node
heapLookup (MkGMHeap (MkHeap size free cts)) a = 
  aLookup cts a


heapAddressesses :: GMHeap -> [Address]
heapAddressesses (MkGMHeap (MkHeap size free cts)) = aDomain cts


heapSize :: GMHeap -> Int
heapSize (MkGMHeap (MkHeap size free cts)) = size

heapNull :: Address
heapNull = 0

heapIsNull :: Address -> Bool
heapIsNull a = a == 0

showAddress :: Address -> String
showAddress a = "#" ++ show a



 