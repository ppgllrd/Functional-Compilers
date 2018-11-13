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
tagFalse 	= 1::Int -- 1 is tag of False
tagTrue  	= 2::Int -- 2 is tag of True

-- Lists
tagNil		= 3::Int -- 3 is tag of Nil
tagCons		= 4::Int -- 4 is tag of Cons

-- Tuples
tagTuple2	= 5::Int -- 5 is tag of ( , )
tagTuple3	= 6::Int -- 5 is tag of ( , )
