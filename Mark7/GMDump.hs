-------------------------------------------------------
-- G-Machine dump
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------

module GMDump ( 
         GMDump,
         dumpEmpty,
         dumpPop,
         dumpPush,
         dumpToList
       ) where


import GMSStack
import GMCode


newtype GMDump     = MkGMDump [GMDumpItem]

newtype GMDumpItem = MkGMDumpItem (GMCode, GMSStack)

dumpEmpty :: GMDump
dumpEmpty = MkGMDump []

dumpToList :: GMDump -> [(GMCode, GMSStack)]
dumpToList (MkGMDump xs) = [x | MkGMDumpItem x <- xs]


dumpPop :: GMDump -> Maybe (GMDump, GMCode, GMSStack)
dumpPop (MkGMDump []                           ) = Nothing
dumpPop (MkGMDump (MkGMDumpItem(code,stack):ds)) = Just (MkGMDump ds,code,stack)

dumpPush :: GMDump -> (GMCode,GMSStack) -> GMDump
dumpPush (MkGMDump ds) (code,stack) = MkGMDump (MkGMDumpItem(code,stack):ds)
