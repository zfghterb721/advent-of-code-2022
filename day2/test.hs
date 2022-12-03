import Data.Map (Map)
import qualified Data.Map as Map

data HandShape = Rock | Paper | Scissors deriving (Read, Show, Enum, Eq, Ord)

data Result = Win | Lose | Draw deriving (Read, Show, Eq, Ord, Enum)

game :: HandShape -> HandShape -> Result
game Rock Scissors = Win
game Paper Rock = Win
game Scissors Paper = Win
game Rock Paper = Lose
game Paper Scissors = Lose
game Scissors Rock = Lose
game _ _ = Draw

decryptHand :: String -> HandShape
decryptHand "A" = Rock
decryptHand "B" = Paper
decryptHand "C" = Scissors
decryptHand "X" = Rock
decryptHand "Y" = Paper
decryptHand "Z" = Scissors

winPoints :: Result -> Integer
winPoints Win = 6
winPoints Lose = 0
winPoints Draw = 3

handPoints :: HandShape -> Integer
handPoints Rock = 1
handPoints Paper = 2
handPoints Scissors = 3

runGame :: HandShape -> HandShape -> Integer
runGame a b = (winPoints (game a b)) + (handPoints a)

sumArray :: [Integer] -> Integer
sumArray [] = 0
sumArray (x : xs) = x + sumArray xs

main = do
  contents <- readFile "data.txt"
  let lineList = lines contents
  let pairs = map words lineList
  let decryptedTest = map (map decryptHand) pairs
  let results = map (\[a, b] -> runGame b a) decryptedTest

  print decryptedTest
  print results
  print (sumArray results)