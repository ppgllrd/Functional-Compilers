-------------------------------------------------------
-- G-Machine statistics
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------


module GMStats (
        GMStats,
        statInitial,
        statIncSteps,
        statGetSteps,
        statIncHeaps,
        statGetHeaps,
        statUpdateSStacks,
        statGetSStacks
       ) where


newtype GMStats   = MkGMStats (Int,Int,Int)

statInitial :: GMStats
statInitial = MkGMStats (0,0,0)

statIncSteps :: GMStats -> GMStats
statIncSteps (MkGMStats (s,h,st)) = MkGMStats ((s+1),h,st)

statGetSteps :: GMStats -> Int
statGetSteps (MkGMStats (s,h,st)) = s

statIncHeaps :: GMStats -> GMStats
statIncHeaps (MkGMStats (s,h,st)) = MkGMStats (s,(h+1),st)

statGetHeaps :: GMStats -> Int
statGetHeaps (MkGMStats (s,h,st)) = h

statUpdateSStacks :: GMStats -> Int -> GMStats
statUpdateSStacks (MkGMStats (s,h,st)) st' = MkGMStats (s,h,(max st st'))

statGetSStacks :: GMStats -> Int
statGetSStacks (MkGMStats (s,h,st)) = st
