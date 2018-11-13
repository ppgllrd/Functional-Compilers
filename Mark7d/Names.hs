-------------------------------------------------
-- José E. Gallardo, August 1997
--
-- An implementation of the name supply type
--
--  "Implementing Functional languages"
--
-------------------------------------------------


module Names (
         NameSupply,
         getName, getNames, 
         initialNameSupply 
       ) where


type NameSupply = Int

getName  :: NameSupply -> [Char]   -> (NameSupply, [Char])
getName name_supply prefix = (name_supply+1, makeName prefix name_supply)

getNames :: NameSupply -> [[Char]] -> (NameSupply, [[Char]])
getNames name_supply prefixes
 = (name_supply + length prefixes, zipWith makeName prefixes [name_supply..])

initialNameSupply :: NameSupply
initialNameSupply = 0

makeName prefix ns = prefix ++ "$" ++ show ns

