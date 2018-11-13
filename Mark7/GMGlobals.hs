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
         globalLookup, globalLookupFromAddress,
         globalFromList, globalToList,
         globalInitial, 
         globalAdd
       ) where

import GlobalDefs
import LibAssoc
import GMHeap

newtype GMGlobals = MkGMGlobals (Assoc Name Address)


globalInitial :: GMGlobals
globalInitial = MkGMGlobals aEmpty

globalLookup :: GMGlobals -> Name -> Maybe Address
globalLookup (MkGMGlobals aGlobals) f = aLookup aGlobals f 

globalLookupFromAddress :: GMGlobals -> Address -> Maybe Name
globalLookupFromAddress (MkGMGlobals aGlobals) a = 
  case [n | (n,a') <- aToList aGlobals, a==a'] of
    []       -> Nothing
    (name:_) -> Just name

globalFromList :: [(Name,Address)] -> GMGlobals
globalFromList xs = MkGMGlobals (aFromList xs)

globalToList :: GMGlobals -> [(Name,Address)]
globalToList (MkGMGlobals globals) = aToList globals

globalAdd :: GMGlobals -> (Name, Address) -> GMGlobals
globalAdd (MkGMGlobals globals) (name,address) = MkGMGlobals (aAdd name address globals)