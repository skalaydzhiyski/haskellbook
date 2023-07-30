module CipherKeyword where
import Data.Char

-- helpers
keyword     = "ALLY"
cipher      = repeat keyword
mapping s   = zip s (map ord $ concat cipher)
start = ord 'a'
end   = ord 'z'

-- This is the main shift function for our cipher from the book.
-- shift :: (Int -> Int -> Int) -> (Char, Int) -> Int
shift :: (Int -> Int -> Int) -> (Char, Int) -> Int
shift f x = if res >= start && res <= end then res else start + (res - start) `mod` 26
  where cord   = ord $ fst x
        offset = snd x
        res    = cord `f` offset

encrypt :: String -> String
encrypt s = map (chr . shift (+)) $ mapping s

decrypt :: String -> String
decrypt s = map (chr . shift (-)) $ mapping s

run_ :: IO ()
run_ = do
  putStrLn "enter string: "
  line <- getLine
  let enc = encrypt line
  putStrLn $ "encrypted: " ++ enc
  let dec = decrypt enc
  putStrLn $ "decrypted: " ++ dec

