import Data.Char
import Data.List

type Digit   = Char     -- the first digit on a keypad button
type Options = String   -- list of options for a given keypad button
type Presses = Int
type Button  = (Digit, Options)

data Phone = Phone [Button] deriving (Show)

type Action  = (Digit, Presses)

options :: [Options]
options = ["1", "abc2", "def3", "ghi4", "jkl5", "mno6", "pqrs7", "tuv8", "wxyz9", "*^", "+ 0", "#.,"]

digits :: [Digit]
digits  = "123456789*0#"

upper :: Action
upper = ('*',1)

phone = Phone (zip digits options)

findButton :: Phone -> Char -> Button
findButton (Phone (x:xs)) c
  | elem c $ snd x = x
  | otherwise      = findButton (Phone xs) c

makeAction :: Button -> Char -> Action
makeAction (btn,chars) c_ = case elemIndex c_ chars of
                              Nothing -> error "No char"
                              Just v  -> (btn, v+1)

parseChar :: Phone -> Char -> [Action]
parseChar p@(Phone buttons) c
  | isLower c = [makeAction (findButton p c) c]
  | otherwise = upper:[makeAction (findButton p lower) lower]
  where lower = toLower c

-- TODO: Continue with the rest of the tasks about the phones exercise from the book.
solve :: Phone -> String -> [Action]
solve p s = concat $ map (parseChar p) s


main :: IO ()
main = undefined

