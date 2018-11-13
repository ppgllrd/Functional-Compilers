-------------------------------------------------------
-- G-Machine stack
--
-- "Implementing Functional Languajes"
-- S. Peyton Jones and D. Lester
--
-- José E. Gallardo, December 1997
-------------------------------------------------------

module LibStack (
         Stack,
         stackEmpty,
         stackPush,
         stackPop,
         stackSelect,
         stackLength,
         stackToList,
         stackFromList
       ) where


newtype Stack a = MkStack [a]

stackEmpty :: Stack a
stackEmpty = MkStack []

stackPush :: Stack a -> a -> Stack a
stackPush (MkStack stack) a = MkStack (a:stack)

stackPop :: Stack a -> Maybe (Stack a, a)
stackPop (MkStack [])     = Nothing
stackPop (MkStack (a:as)) = Just ((MkStack as), a)

stackLength :: Stack a -> Int
stackLength (MkStack stack) = length stack

stackSelect :: Stack a -> Int -> Maybe a
stackSelect (MkStack [])     _     = Nothing
stackSelect (MkStack (a:as)) 0     = Just a
stackSelect (MkStack (a:as)) n | n>0 = stackSelect (MkStack as) (n-1)

stackToList :: Stack a -> [a]
stackToList (MkStack as) = as

stackFromList :: [a] -> Stack a
stackFromList xs = MkStack xs