*****************************************************************************
Haskell Script : BalancedTree.lhs
Author         : Benedict R. Gaster
Date           : 19th July 1995
Desc           : A implemation of blanced trees
Note           : If using Gofer or Hugs, load the file Maybe.lhs before loading
	       : this one.
*****************************************************************************

> module LibBalancedTree (
>	BalancedTree, 			 	-- Abstract data type

>	emptyBT, singletonBT, listToBT,  	-- Building

>	addToBT, addListToBT, delFromBT, 	-- Adding and Deleting
>	delListFromBT, 

>       intersectBT, plusBT, minusBT,		-- Combining

>	foldBT, mapBT, filterBT,		-- Mapping, Folding, Filtering

>	sizeBT, lookupBT, lookupWithDefaultBT,	-- Interrogating

>	btToList, 				-- Listifying

>	Maybe  					-- To make it self-sufficient

>	) where

--------------------------------------------------------------------------

> data (Eq a, Ord a) => BalancedTree a = EmptyBT
>				       | BranchBT Int (BalancedTree a) a 
>						      (BalancedTree a)
>	deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------

> valueBT :: Ord a => BalancedTree a -> a
> valueBT EmptyBT = error "Error: Balanced tree is empty"
> valueBT (BranchBT _ _ v _) = v

--------------------------------------------------------------------------

> emptyBT :: (Eq a, Ord a) => BalancedTree a
> emptyBT = EmptyBT

> singletonBT :: (Eq a, Ord a) => a -> BalancedTree a
> singletonBT item = BranchBT 1 EmptyBT item EmptyBT

> listToBT :: (Eq a, Ord a) => [a] -> BalancedTree a
> listToBT = foldr addToBT EmptyBT

--------------------------------------------------------------------------

> sBranchBT :: (Eq a, Ord a) => BalancedTree a -> a ->
>				BalancedTree a -> 
>				BalancedTree a
> sBranchBT l v r = BranchBT ((sizeBT l) + (sizeBT r) + 1) l v r

--------------------------------------------------------------------------

> singleLeft :: (Eq a, Ord a) => a -> BalancedTree a ->
>				      BalancedTree a -> 
>				      BalancedTree a
> singleLeft a x (BranchBT _ y b z) = sBranchBT (sBranchBT x a y) b z

> doubleLeft :: (Eq a, Ord a) => a -> BalancedTree a ->
>				      BalancedTree a -> 
>				      BalancedTree a
> doubleLeft a x (BranchBT _ (BranchBT _ y1 b y2) c z) =
>	sBranchBT (sBranchBT x a y1) b (sBranchBT y2 c z)

--------------------------------------------------------------------------

> singleRight :: (Eq a, Ord a) => a -> BalancedTree a ->
>				       BalancedTree a -> 
>				       BalancedTree a
> singleRight b (BranchBT _ x a y) z = sBranchBT x a (sBranchBT y b z)

> doubleRight :: (Eq a, Ord a) => a -> BalancedTree a ->
>				       BalancedTree a -> 
>				       BalancedTree a
> doubleRight c (BranchBT _ x a (BranchBT _ y1 b y2)) z =
>	sBranchBT (sBranchBT x a y1) b (sBranchBT y2 c z)

--------------------------------------------------------------------------

> balanceBT l v r | ln + rn < 2 = sBranchBT l v r
>		  | rn > ratio * ln = let (BranchBT _ rl _ rr) = r
>				          rln = sizeBT rl
>				          rrn = sizeBT rr
>				      in
>				          if rln < rrn then singleLeft v l r
>				          else              doubleLeft v l r
>		  | ln > ratio * rn = let (BranchBT _ ll _ lr) = l
>				          lln = sizeBT ll
>				          lrn = sizeBT lr
>				      in
>				          if lrn < lln then singleRight v l r
>				          else              doubleRight v l r
>		  | otherwise       = sBranchBT l v r
>	where
>	ratio = 5
>	ln = sizeBT l
>	rn = sizeBT r

--------------------------------------------------------------------------

Building balanced trees...........................

> addToBT :: (Eq a, Ord a) => a -> BalancedTree a -> BalancedTree a
> addToBT v EmptyBT                             = singletonBT v
> addToBT v tree@(BranchBT n l a r) | v < a     = balanceBT (addToBT v l) a r
> 				    | v > a     = balanceBT l a (addToBT v r)
>				    | otherwise = BranchBT n l v r

> addListToBT :: (Eq a, Ord a) => BalancedTree a -> [a] -> BalancedTree a
> addListToBT = foldr addToBT



The following function is used by "delFromBT" and "minusBT" to join two
balanced trees together. The trees must have the properties that the
first has elements that are all less than all elements in the second.

> joinBT :: (Eq a, Ord a) => BalancedTree a -> BalancedTree a -> BalancedTree a
> joinBT EmptyBT r       = r
> joinBT l       EmptyBT = l
> joinBT l       r       = balanceBT l v' r'
>	where
>	(v',r') = minBT r
> 	minBT (BranchBT _ EmptyBT v r) = (v,r)
> 	minBT (BranchBT _ l v r)       = (y, balanceBT t2 v r)
>		where
>		(y,t2) = minBT l

> delFromBT :: (Eq a, Ord a) => a -> BalancedTree a -> BalancedTree a
> delFromBT _ EmptyBT                    = EmptyBT
> delFromBT v (BranchBT _ l a r) | v < a = balanceBT (delFromBT v l) a r
>                                | v > a = balanceBT l a (delFromBT v r)
>                                | otherwise    = joinBT l r

> delListFromBT :: (Eq a, Ord a) => BalancedTree a -> [a] -> BalancedTree a
> delListFromBT = foldr delFromBT

--------------------------------------------------------------------------

> concat3 :: (Eq a, Ord a) => BalancedTree a -> a ->
>                             BalancedTree a ->
>                             BalancedTree a
> concat3 EmptyBT                  v r       = addToBT v r
> concat3 l                        v EmptyBT = addToBT v l
> concat3 l@(BranchBT s1 l1 v1 r1) v r@(BranchBT s2 l2 v2 r2) 
>		| ratio * s1 < s2 = balanceBT (concat3 l v l2) v2 r2
>               | ratio * s2 < s1 = balanceBT l1 v1 (concat3 r1 v r)
>               | otherwise       = sBranchBT l v r
>	where
>	ratio = 5

> splitLT :: (Eq a, Ord a) => BalancedTree a -> a -> BalancedTree a
> splitLT EmptyBT            _             = EmptyBT
> splitLT (BranchBT _ l v r) x | x < v     = splitLT l x
>                              | x > v     = concat3 l v (splitLT r x)
>                              | otherwise = l

> splitGT :: (Eq a, Ord a) => BalancedTree a -> a -> BalancedTree a
> splitGT EmptyBT           _             = EmptyBT
> splitGT (BranchBT _ l v r) x | x > v    = splitGT r x
>                              | x < v     = concat3 (splitGT l x) v r
>                              | otherwise = r

Set operations for balanced trees.... 
plusBT      => union of two trees
minusBT     => difference of two trees
intersectBT => intersection of two trees

> plusBT :: (Eq a, Ord a) => BalancedTree a -> 
>                            BalancedTree a ->
>                            BalancedTree a
> plusBT EmptyBT tree2              = tree2
> plusBT tree1   EmptyBT            = tree1
> plusBT tree1   (BranchBT _ l a r) = concat3 (plusBT l' l) a (plusBT r' r)
>	where
>	l' = splitLT tree1 a
>	r' = splitGT tree1 a

> minusBT :: (Eq a, Ord a) => BalancedTree a ->
>                             BalancedTree a ->
>                             BalancedTree a
> minusBT EmptyBT _                  = EmptyBT
> minusBT tree1   EmptyBT            = tree1
> minusBT tree1   (BranchBT _ l a r) = joinBT (minusBT l' l) (minusBT r' r)
>	where
>	l'  = splitLT tree1 a
>	r'  = splitGT tree1 a

> intersectBT :: (Eq a, Ord a) => BalancedTree a ->
>				BalancedTree a ->
>				BalancedTree a
> intersectBT EmptyBT _                = EmptyBT
> intersectBT _       EmptyBT          = EmptyBT
> intersectBT tree1 (BranchBT _ l a r) = 
>	case lookupBT tree1 a of
>		Just a  -> concat3 (intersectBT l' l) a (intersectBT r' r)
>		Nothing -> joinBT  (intersectBT l' l)   (intersectBT r' r)
>	where
>	l'  = splitLT tree1 a
>	r'  = splitGT tree1 a

--------------------------------------------------------------------------

> foldBT :: (Eq a, Ord a) => (a -> b -> b) -> b -> BalancedTree a -> b
> foldBT _ z EmptyBT = z
> foldBT k z (BranchBT _ l v r) = foldBT k (k v (foldBT k z r)) l

> mapBT :: (Eq a, Ord a, Eq b, Ord b) => (a -> b) -> BalancedTree a ->
>                                                    BalancedTree b
> mapBT _ EmptyBT = EmptyBT
> mapBT f (BranchBT _ l a r) = sBranchBT (mapBT f l) (f a) (mapBT f r)

> filterBT :: (Eq a, Ord a) => (a -> Bool) -> BalancedTree a ->
>                                             BalancedTree a
> filterBT _ EmptyBT = EmptyBT
> filterBT f tree@(BranchBT _ l a r) | f a       = balanceBT (filterBT f l) a 
>                                                            (filterBT f r)
>                                    | otherwise = filterBT f (delFromBT a tree) 

--------------------------------------------------------------------------

> lookupBT :: (Eq a, Ord a) => BalancedTree a -> a -> Maybe a
> lookupBT EmptyBT            _             = Nothing
> lookupBT (BranchBT _ l a r) v | v < a     = lookupBT l v
>                               | v > a     = lookupBT r v
>                               | otherwise = Just a

> lookupWithDefaultBT :: (Eq a, Ord a) => BalancedTree a -> a -> a -> a
> lookupWithDefaultBT tree d v = case lookupBT tree v of
>					Just a  -> a
>					Nothing -> d

> sizeBT :: (Eq a, Ord a) => BalancedTree a -> Int
> sizeBT EmptyBT            = 0
> sizeBT (BranchBT n l _ r) = n

--------------------------------------------------------------------------

> btToList :: (Eq a, Ord a) => BalancedTree a -> [a]
> btToList = foldBT (:) []
