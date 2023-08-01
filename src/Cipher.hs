module Cipher where
import Data.Char

-- helpers
shiftWidths = 1 :: Int
cipher = repeat [1..shiftWidths]
start = ord 'a'
end   = ord 'z'
mapping s = zip s (concat cipher)


shift :: (Int -> Int -> Int) -> (Char, Int) -> Int
shift f x = if res >= start && res <= end then res else start + (res - start) `mod` 26
  where cord   = ord $ fst x
        offset = snd x
        res    = cord `f` offset

f :: Int -> Integer -> (Int, Int)
f x y = undefined

encrypt :: String -> String
encrypt s = map (chr . shift (+)) $ mapping s

decrypt :: String -> String
decrypt s = map (chr . shift (-)) $ mapping s

run :: IO ()
run = do
  putStrLn "enter string: "
  line <- getLine
  let enc = encrypt line
  putStrLn $ "encrypted: " ++ enc
  let dec = decrypt enc
  putStrLn $ "decrypted: " ++ dec

