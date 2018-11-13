-------------------------------------------------
-- José E. Gallardo, July 1997
--
-- An implementation of the association list type
--
--  "Implementing Functional languages"
--
-------------------------------------------------

> module LibAssoc (

>	Assoc(..),

> 	aFromList, aEmpty, aSingleton, aAdd,

>	aToList,

>	aUnion, aUnionList, aIntersection,
>	aSubtraction,

>	aElementOf, aIsEmpty,

>	aLookup, aLookupWithDefault,

>	aFold,

>	aRange, aDomain,

>       aLength,

>	) where

> import LibBalancedTree

> data Pair a b = MkPair a b
>	deriving (Show,Read)

> instance Eq a => Eq (Pair a b) where
>	MkPair a _ == MkPair b _ = a == b

> instance (Eq a, Ord a) => Ord (Pair a b) where
>	MkPair a _ <= MkPair b _ = a <= b

> newtype Assoc a b = MkAssoc (BalancedTree (Pair a b))

> aAdd :: Ord a => a -> b -> Assoc a b -> Assoc a b
> aAdd a b (MkAssoc fm) = MkAssoc (addToBT (MkPair a b) fm)

> aFromList :: Ord a => [(a,b)] -> Assoc a b
> aFromList xs = MkAssoc (listToBT (map (\(a,b) -> MkPair a b) xs))

> aToList :: Ord a => Assoc a b -> [(a,b)]
> aToList (MkAssoc fm) = map (\(MkPair a b) -> (a,b)) (btToList fm)

> aEmpty :: Ord a => Assoc a b
> aEmpty = MkAssoc emptyBT

> aSingleton :: Ord a => a -> b -> Assoc a b
> aSingleton a b = MkAssoc (singletonBT (MkPair a b))

> aUnion :: Ord a => Assoc a b -> Assoc a b -> Assoc a b
> aUnion (MkAssoc fm) (MkAssoc fm')  = MkAssoc (plusBT fm fm')

> aUnionList :: Ord a => [Assoc a b] -> Assoc a b
> aUnionList fm = foldl aUnion aEmpty fm

> aIntersection :: Ord a => Assoc a b -> Assoc a b -> Assoc a b
> aIntersection (MkAssoc fm) (MkAssoc fm') = MkAssoc (intersectBT fm fm')

> aSubtraction :: Ord a => Assoc a b -> Assoc a b -> Assoc a b
> aSubtraction (MkAssoc fm) (MkAssoc fm') = MkAssoc (minusBT fm fm')

> aMap :: Ord a => (b -> c) -> Assoc a b -> Assoc a c
> aMap f (MkAssoc fm) = MkAssoc (mapBT (\(MkPair a b) -> (MkPair a (f b))) fm)


> instance Ord a => Functor (Assoc a)  where
>   map = aMap

> aFold :: Ord a => (b -> c -> c) -> c -> Assoc a b -> c
> aFold f e (MkAssoc fm) = foldBT (\(MkPair _ b) c -> f b c) e fm

> aElementOf :: Ord a => a -> Assoc a b -> Bool
> aElementOf v (MkAssoc fm) | aIsEmpty (MkAssoc fm) = False
>             		    | otherwise             = maybe False (\_ -> True)
>				                            (lookupBT fm (MkPair v undefined))
>				       

> aLookup :: Ord a => Assoc a b -> a -> Maybe b
> aLookup (MkAssoc fm) a  = case lookupBT fm (MkPair a undefined) of
>			      Just (MkPair _ b) -> Just b
>	 		      _	                -> Nothing


> aLookupWithDefault :: Ord a => Assoc a b -> a -> b ->  b
> aLookupWithDefault fm a b = case aLookup fm a of
>			Just v  -> v
>			Nothing -> b 

> aIsEmpty :: Ord a => Assoc a b -> Bool
> aIsEmpty (MkAssoc fm) = sizeBT fm == 0


> aDomain :: Ord a => Assoc a b -> [a]
> aDomain fm = [ a | (a,b) <- aToList fm]


> aRange :: Ord a => Assoc a b -> [b]
> aRange fm = [ b | (a,b) <- aToList fm]

> aLength :: Ord a => Assoc a b -> Int
> aLength (MkAssoc fm) = sizeBT fm