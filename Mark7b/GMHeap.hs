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
         Addr,
         heapInitial, heapAlloc, heapUpdate, heapFree, heapLookup,
         heapSize, heapNull, heapIsNull,
         heapAddresses,
         showAddr
       ) where
 

import GMCode
import LibAssoc
import List

data Heap a = MkHeap Int [Int] (Assoc Int a)

newtype GMHeap  = MkGMHeap  (Heap Node)

data Node = NNum Int		-- Numbers
	  | NAp Addr Addr	-- Applications
	  | NGlobal Int GMCode  -- Global definitions
          | NInd Addr           -- Indirections
          | NConstr Int [Addr]  -- Constructors
          

type Addr   = Int

heapInitial :: GMHeap 
heapInitial = MkGMHeap (MkHeap 0 [1..] aEmpty)


heapAlloc :: GMHeap -> Node -> (GMHeap, Addr)
heapAlloc (MkGMHeap (MkHeap size (next:free) cts)) n = (MkGMHeap (MkHeap (size+1) free (aUnion (aSingleton next n) cts)), next)


heapUpdate :: GMHeap -> Addr -> Node -> GMHeap
heapUpdate (MkGMHeap (MkHeap size free cts)) a n = MkGMHeap (MkHeap size free (aUnion newA (aSubtraction cts oldA)))
  where newA = aSingleton a n
        oldA = let (Just oldN) = aLookup cts a
               in aSingleton a oldN


heapFree :: GMHeap -> Addr -> GMHeap
heapFree (MkGMHeap (MkHeap size free cts)) a = MkGMHeap (MkHeap (size-1) (a:free) (aSubtraction cts oldA))
  where oldA = let (Just oldN) = aLookup cts a
               in aSingleton a oldN


heapLookup :: GMHeap -> Addr -> Maybe Node
heapLookup (MkGMHeap (MkHeap size free cts)) a = 
  aLookup cts a


heapAddresses :: GMHeap -> [Addr]
heapAddresses (MkGMHeap (MkHeap size free cts)) = aDomain cts


heapSize :: GMHeap -> Int
heapSize (MkGMHeap (MkHeap size free cts)) = size

heapNull :: Addr
heapNull = 0

heapIsNull :: Addr -> Bool
heapIsNull a = a == 0

showAddr :: Addr -> String
showAddr a = "#" ++ show a



 