import Data.List

-- examples on as-patterns (can be used to, for example, expand a list and then still be able to
--  refer to the list as a list and not (x:xs).. here we don't have access to the full list
--  while here -> lst@(x:xs) , we have access to both representations

f1 :: Show a => (a,b) -> IO (a,b)
f1 xs@(x,_) = do
  print x
  return xs

doubleUp :: [a] -> [a]
doubleUp lst@(x:_) = x : lst

-- TODO: use as-patterns to implement the subsequence problem from the book.

main :: IO ()
main = undefined

