-------------------------------------------------------
-- Global definitions
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------
 
module GlobalDefs (
	 Name,
	 BasicValue(..)
       ) where


-- Names of supercombinators
type Name = String

-- Basic values
data BasicValue = BasicChar Char 
                | BasicInt Int
                deriving (Eq,Show)