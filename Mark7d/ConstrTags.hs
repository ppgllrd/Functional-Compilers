-------------------------------------------------------
-- Tags for constructors defined in prelude
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------
 
module ConstrTags (
	 tagTrue, tagFalse,
	 tagNil, tagCons,
	 tagTuple2, tagTuple3
       ) where


-- Booleans
tagFalse 	= 1 -- 1 is tag of False
tagTrue  	= 2 -- 2 is tag of True

-- Lists
tagNil		= 3 -- 3 is tag of Nil
tagCons		= 4 -- 4 is tag of Cons

-- Tuples
tagTuple2	= 5 -- 5 is tag of ( , )
tagTuple3	= 6 -- 5 is tag of ( , )
