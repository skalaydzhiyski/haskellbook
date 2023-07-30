module AsPatterns where

import Data.Char
-- examples on as-patterns (can be used to, for example, expand a list and then still be able to
--  refer to the list as a list and not (x:xs).. here we don't have access to the full list
--  while here -> lst@(x:xs) , we have access to both representations

f1 :: Show a => (a,b) -> IO (a,b)
f1 xs@(x,_) = do
  print x
  return xs
  -- with thi sbrother 

doubleUp :: [a] -> [a]
doubleUp lst@(x:_) = x : lst

-- Solution to the subsequence problem from the book.
isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf (x:_) [] = False
isSubseqOf [] _     = True
isSubseqOf l1@(x:xs) l2@(y:ys)
  | x /= y    = isSubseqOf l1 ys
  | otherwise = isSubseqOf xs ys

capitailizeWords :: String -> [(String, String)]
capitailizeWords s = map capitalizeToTuple $ words s
  where capitalizeToTuple word@(c:cs) =
          let upperFirst = chr $ (ord c) - 32
           in (upperFirst:cs, word)


