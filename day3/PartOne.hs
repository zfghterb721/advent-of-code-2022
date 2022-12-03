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

getCommonChars :: (String, String) -> Char
getCommonChars ([], b) = ' '
getCommonChars ((x : xs), b) = if checkForChar x b then x else getCommonChars (xs, b)

convertCharToRank :: Char -> Int
convertCharToRank a = if (t <= 0x7A) && (t >= 0x61) then (t - 0x60) else (t - 0x40 + 26)
  where
    t = fromEnum a

sumArray :: [Int] -> Int
sumArray [] = 0
sumArray (x : xs) = x + sumArray xs

main = do
  contents <- readFile "data.txt"
  let lineList = lines contents
  print (sumArray (map convertCharToRank (map getCommonChars (map (splitStringInHalf) lineList))))
