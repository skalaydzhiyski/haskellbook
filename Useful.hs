
-- Read multiple lines from the standard input
import Control.Monad
main :: IO ()
main = do
  n <- getLine
  lines <- replicateM (read n :: Int) getLine -- list of strings
  putStrLn "worked"
