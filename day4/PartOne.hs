import Data.Text.IO (readFile)
import Data.Text (Text, pack, splitOn)
import Data.Text.Read (decimal)
import Data.Either
import Distribution.Compat.Lens (_1)


main = do
  contents <- Data.Text.IO.readFile "data.txt"
  let result = map
        (map (splitOn (pack "-")) . splitOn (pack ","))
        (splitOn (pack "\n") contents)

  let parsedResult = (map.map.map)
        ((fst . fromRight (0, pack "") . decimal)) result

  let solvedResult = (map)
        isOverlappingArray parsedResult

  print parsedResult
  print solvedResult
  print (countTrue solvedResult)

isFullyContainedArray :: [[Integer]] -> Bool
isFullyContainedArray [[]] = False
isFullyContainedArray [(a0 : a1 : a), (b0 : b1 : b)] = isFullyContained [a0, a1] [b0, b1]

isOverlappingArray :: [[Integer]] -> Bool
isOverlappingArray [[]] = False
isOverlappingArray [(a0 : a1 : a), (b0 : b1 : b)] = isOverlapping [a0, a1] [b0, b1]

countTrue :: [Bool] -> Int
countTrue [] = 0
countTrue (x:xs) = if x then 1 + countTrue xs else countTrue xs


isFullyContained :: [Integer] -> [Integer] -> Bool
isFullyContained [] _ = False
isFullyContained _ [] = False
isFullyContained [_] _ = False
isFullyContained _ [_] = False
isFullyContained (a1:a2:_) (b1:b2:_) = ((a1 >= b1) && (a2 <= b2)) || ((a1 <= b1) && (a2 >= b2))

isOverlapping :: [Integer] -> [Integer] -> Bool
isOverlapping [] _ = False
isOverlapping _ [] = False
isOverlapping [_] _ = False
isOverlapping _ [_] = False
isOverlapping (a1 : a2 : _) (b1 : b2 : _) = b2 >= a1 && a1 >= b1 || a2 >= b1 && b1 >= a1