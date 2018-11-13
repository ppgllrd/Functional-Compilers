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
        statGetSteps
       ) where


newtype GMStats   = MkGMStats Int

statInitial :: GMStats
statInitial = MkGMStats 0

statIncSteps :: GMStats -> GMStats
statIncSteps (MkGMStats s) = MkGMStats (s+1)

statGetSteps :: GMStats -> Int
statGetSteps (MkGMStats s) = s
 