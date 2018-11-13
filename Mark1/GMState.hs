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
import GMStack
import GMHeap
import GMGlobals
import GMStats


-- G-Machine State
data GMState = MkGMState { gmCode    :: GMCode,	        -- Current instruction stream
			   gmStack   :: GMStack,	-- Current Stack
			   gmHeap    :: GMHeap,	        -- Heap of nodes
			   gmGlobals :: GMGlobals,	-- Global addresses in heap
			   gmStats   :: GMStats }       -- Statistics	


