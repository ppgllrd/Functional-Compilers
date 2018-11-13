-------------------------------------------------------
-- G-Machine global names
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------


module GMGlobals (
         GMGlobals,
         globalLookup, globalLookupFromAddr,
         globalFromList, globalToList,
         globalInitial, 
         globalAdd
       ) where

import GlobalDefs
import LibAssoc
import GMHeap

newtype GMGlobals = MkGMGlobals (Assoc Name Addr)


globalInitial :: GMGlobals
globalInitial = MkGMGlobals aEmpty

globalLookup :: GMGlobals -> Name -> Maybe Addr
globalLookup (MkGMGlobals aGlobals) f = aLookup aGlobals f 

globalLookupFromAddr :: GMGlobals -> Addr -> Maybe Name
globalLookupFromAddr (MkGMGlobals aGlobals) a = 
  case [n | (n,a') <- aToList aGlobals, a==a'] of
    []       -> Nothing
    (name:_) -> Just name

globalFromList :: [(Name,Addr)] -> GMGlobals
globalFromList xs = MkGMGlobals (aFromList xs)

globalToList :: GMGlobals -> [(Name,Addr)]
globalToList (MkGMGlobals globals) = aToList globals

globalAdd :: GMGlobals -> (Name, Addr) -> GMGlobals
globalAdd (MkGMGlobals globals) (name,addr) = MkGMGlobals (aAdd name addr globals)