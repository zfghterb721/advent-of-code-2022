import Data.Text (chunksOf)
import Data.List (nub)

getLength :: [a] -> Int
getLength [] = 0
getLength (_ : xs) = 1 + getLength xs

splitStringInHalf :: String -> (String, String)
splitStringInHalf s = splitAt half s
  where
    half = getLength s `div` 2

checkForChar :: Char -> String -> Bool
checkForChar a [] = False
checkForChar a (x : xs) = (a == x) || checkForChar a xs

getCommonChars :: (String, String) -> String -> String
getCommonChars ([], b) acc = acc
getCommonChars ((x : xs), b) acc = if checkForChar x b then getCommonChars (xs, b) (acc ++ [x]) else getCommonChars (xs, b) acc

getCommonCharsArray :: String -> [String] -> String
getCommonCharsArray acc [] = acc
getCommonCharsArray acc [a] = acc
getCommonCharsArray acc [a, b]  = getCommonChars (a, b) ""
getCommonCharsArray acc (x : x' : xs) = getCommonCharsArray acc (getCommonChars (x, x') "" : xs)

convertCharToRank :: Char -> Int
convertCharToRank a = if (t <= 0x7A) && (t >= 0x61) then (t - 0x60) else (t - 0x40 + 26)
  where
    t = fromEnum a

sumArray :: [Int] -> Int
sumArray [] = 0
sumArray (x : xs) = x + sumArray xs

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

main = do
  contents <- readFile "data.txt"
  let lineList = lines contents
  let newList = group 3 lineList
  let commons = map (getCommonCharsArray "")newList
  let simple =  map convertCharToRank (map head ((map nub commons)))
  let sum = sumArray simple
  print sum

