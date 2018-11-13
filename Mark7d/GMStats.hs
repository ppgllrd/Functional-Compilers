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
        statGetSStacks,
        statUpdateVStacks,
        statGetVStacks,
        statUpdateDumps,
        statGetDumps
       ) where


newtype GMStats   = MkGMStats (Int,Int,Int,Int,Int)

statInitial :: GMStats
statInitial = MkGMStats (0,0,0,0,0)

statIncSteps :: GMStats -> GMStats
statIncSteps (MkGMStats (s,h,st,vt,d)) = MkGMStats ((s+1),h,st,vt,d)

statGetSteps :: GMStats -> Int
statGetSteps (MkGMStats (s,h,st,vt,d)) = s

statIncHeaps :: GMStats -> GMStats
statIncHeaps (MkGMStats (s,h,st,vt,d)) = MkGMStats (s,(h+1),st,vt,d)

statGetHeaps :: GMStats -> Int
statGetHeaps (MkGMStats (s,h,st,vt,d)) = h

statUpdateSStacks :: GMStats -> Int -> GMStats
statUpdateSStacks (MkGMStats (s,h,st,vt,d)) st' = MkGMStats (s,h,(max st st'),vt,d)

statGetSStacks :: GMStats -> Int
statGetSStacks (MkGMStats (s,h,st,vt,d)) = st

statUpdateVStacks :: GMStats -> Int -> GMStats
statUpdateVStacks (MkGMStats (s,h,st,vt,d)) vt' = MkGMStats (s,h,st,(max vt vt'),d)

statGetVStacks :: GMStats -> Int
statGetVStacks (MkGMStats (s,h,st,vt,d)) = vt

statUpdateDumps :: GMStats -> Int -> GMStats
statUpdateDumps (MkGMStats (s,h,st,vt,d)) d' = MkGMStats (s,h,st,vt,(max d d'))

statGetDumps :: GMStats -> Int
statGetDumps (MkGMStats (s,h,st,vt,d)) = d
