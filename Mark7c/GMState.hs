-------------------------------------------------------
-- Mark 1 Machine State
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------

module GMState (
        GMState(..)
       ) where

import GMCode
import GMSStack
import GMVStack
import GMHeap
import GMGlobals
import GMStats
import GMDump


-- G-Machine State
data GMState = MkGMState { gmCode    :: GMCode,	        -- Current instruction stream
			   gmSStack  :: GMSStack,	-- Current S-Stack
			   gmVStack  :: GMVStack,	-- Current V-Stack
                           gmDump    :: GMDump,         -- Current Dump
			   gmHeap    :: GMHeap,	        -- Heap of nodes
			   gmGlobals :: GMGlobals,	-- Global addresses in heap
			   gmStats   :: GMStats }       -- Statistics	


