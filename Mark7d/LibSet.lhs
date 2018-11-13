-------------------------------------------------
-- José E. Gallardo, July 1997
--
-- An implementation of the Set type
--
--  "Implementing Functional languages"
--
-------------------------------------------------

> module LibSet (

>	Set,

> 	setFromList, setEmpty, setSingleton,

>	setToList,

>	setUnion, setUnionLists, setIntersection,
>	setSubtraction,

>	setMap,

>	setElementOf, setIsEmpty,

>	Maybe, 					-- To make it self-sufficient
>	BalancedTree 			 	-- To make it self-sufficient

>	) where

> import LibBalancedTree

> data (Eq a, Ord a) => Set a = MkSet (BalancedTree a)
>	deriving (Eq, Ord) -- No Text instance here, see below.

Now define out own text instance for set, thus allowing us to read and
display the more standard list notation. E.g. {1 ... n} ordered set of n ints.

> instance (Ord a, Read a) => Read (Set a) where
>	readsPrec _ = readParen False (\r -> 
>			[ (MkSet (listToBT l),r') | ("{",s) <- lex r,
>			                            (l,r')  <- readl s])
>		where
>		readl s =  [([],t)   | ("}",t) <- lex s] ++
>			   [(x:xs,u) | (x,t)   <- reads s,
>				       (xs,u)  <- readl' t]
>		readl' s = [([],t)   | ("}",t) <- lex s] ++
>			   [(x:xs,v) | (" ",t) <- l s,
>				       (x,u)   <- reads t,
>				       (xs,v)  <- readl' u]
>		l (' ':xs) = [(" ", xs)]	-- Need as lex enores spaces..
>       	l s        = lex s


> instance (Ord a, Show a) => Show (Set a) where
>	showsPrec _ (MkSet t) = case sizeBT t == 0 of
>					True  -> showString "{}"			
>					False -> showChar '{' . shows x . s xs
>		where
>		(x:xs)   = btToList t
>		s []     = showChar '}'
>		s (x:xs) = showChar ' ' . shows x . s xs

--------------------------------------------------------------------------

> setFromList :: Ord a => [a] -> Set a
> setFromList xs = MkSet (listToBT xs)

> setToList :: Ord a => Set a -> [a]
> setToList (MkSet t) = btToList t

> setEmpty :: Ord a => Set a
> setEmpty = MkSet emptyBT

> setSingleton :: Ord a => a -> Set a
> setSingleton v = MkSet (singletonBT v)

> setUnion :: Ord a => Set a -> Set a -> Set a
> setUnion (MkSet t1) (MkSet t2) = MkSet (plusBT t1 t2)

> setUnionLists :: Ord a => [Set a] -> Set a
> setUnionLists = foldl setUnion setEmpty

> setIntersection :: Ord a => Set a -> Set a -> Set a
> setIntersection (MkSet t1) (MkSet t2) = MkSet (intersectBT t1 t2)

> setSubtraction :: Ord a => Set a -> Set a -> Set a
> setSubtraction (MkSet t1) (MkSet t2) = MkSet (minusBT t1 t2)

> setMap :: (Ord a,Ord b) => (b -> a) -> Set b -> Set a
> setMap f (MkSet t1) = MkSet (mapBT f t1)

> setElementOf :: Ord a => a -> Set a -> Bool
> setElementOf v (MkSet t) = maybe False (\_ -> True) (lookupBT t v)

> setIsEmpty :: Ord a => Set a -> Bool
> setIsEmpty (MkSet t) = sizeBT t == 0
