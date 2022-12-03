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

decryptResult :: String -> Result
decryptResult "X" = Lose
decryptResult "Y" = Draw
decryptResult "Z" = Win



winPoints :: Result -> Integer
winPoints Win = 6
winPoints Lose = 0
winPoints Draw = 3

handPoints :: HandShape -> Integer
handPoints Rock = 1
handPoints Paper = 2
handPoints Scissors = 3

getHandForOutcome :: HandShape -> Result -> HandShape
getHandForOutcome Rock Lose = Scissors
getHandForOutcome Paper Lose = Rock
getHandForOutcome Scissors Lose = Paper
getHandForOutcome Rock Win = Paper
getHandForOutcome Paper Win = Scissors
getHandForOutcome Scissors Win = Rock
getHandForOutcome a _ = a

runGame :: [String] -> Integer
runGame [] = 0
runGame [_] = 0
runGame (a : b : _) = (winPoints (decryptResult b)) + (handPoints (getHandForOutcome (decryptHand a) (decryptResult b)))

sumArray :: [Integer] -> Integer
sumArray [] = 0
sumArray (x : xs) = x + sumArray xs

main = do
  contents <- readFile "data.txt"
  let lineList = lines contents
  let pairs = map words lineList
  let results = map runGame pairs

  print results
  print (sumArray results)