module Main where

import Control.Monad (join)

f :: (Monad m, Show a) => a -> m String
f = (return . show)

-- this is basically (flip (>>=)) for when testing exmaples.
bind :: Monad m => (a -> m b) -> m a -> m b
bind f mx = join $ fmap f mx


-- binding and binding' should have the output in the console
binding :: IO ()
binding = do
  value <- getLine
  value2 <- getLine
  putStrLn $ value ++ ['\n'] ++ value2

binding' :: IO ()
binding' = getLine >>= putStrLn >> getLine >>= putStrLn


bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "Enter something else here: "
  value <- getLine
  putStrLn $ "Your name is: " ++ value

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "Enter something else here: " >> getLine >>= (\value -> putStrLn $ "Your name is: " ++ value)


nameAge :: IO ()
nameAge = do
  putStrLn "enter your name: "
  name <- getLine
  putStrLn "enter your age: "
  age <- getLine
  putStrLn $ name ++ "\n" ++ age

-- do syntax is just to avoid the nesting below - behaviour is the same!
nameAge' :: IO ()
nameAge' =
  putStrLn "enter your name: " >>
  getLine >>=
    \name -> putStrLn "enter your age: " >>
      getLine >>=
        \age -> putStrLn $ name ++ "\n" ++ age


-- TODO: investigate the behaviour of this function
doubleEvens :: [Integer] -> [Integer]
doubleEvens xs = do
  x <- xs
  if even x then [x*x, x*x] else [x*x]

main :: IO ()
main = do
  putStrLn "working brother"
