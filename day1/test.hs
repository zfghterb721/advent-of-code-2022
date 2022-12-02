import Data.Text (pack, splitOn, Text)
import Data.Text.Read (decimal)
import Data.Text.IO (readFile)
import Data.Either
import Data.List (maximumBy)

main = do
  contents <- Data.Text.IO.readFile "data.txt"
  let groups = splitOn (pack "\n\n") (contents)
  let groups_split = map (splitOn (pack "\n")) groups
  let groups_cast = map (map (fst . fromRight (0, pack "") . decimal)) groups_split
  let groups_summed = map (sumArray) groups_cast

  let maximum = maxNum groups_summed

  let bigGuy = findIndex maximum groups_summed + 1




  print groups_summed
  print maximum
  print bigGuy

maxNum :: Ord a => [a] -> a
maxNum [x] = x
maxNum (x : x' : xs) = maxNum ((if x >= x' then x else x') : xs)

findIndex :: Integer -> [Integer] -> Integer
findIndex _ [] = 0
findIndex i (x:xs) = if x == i then 0 else 1 + findIndex i xs

sumArray :: [Integer] -> Integer
sumArray [] = 0
sumArray (x:xs) = x + sumArray xs
