import Data.Either
import Data.Text (Text, pack, replace, splitOn, unpack)
import Data.Text.IO (readFile)
import Data.Text.Read (decimal)

main = do
  contents <- Data.Text.IO.readFile "data2.txt"
  let [stack, instruction] = splitOn (pack "\n\n") contents
  let stackLines = last (splitOn (pack "\n") stack)
  let indices = buildIndexMap (unpack stackLines)
  let cratesText = reverse (drop 1 (reverse (splitOn (pack "\n") stack)))
  let crates = convertCratesText indices (map unpack cratesText)
  let instructions = map parseInstruction (splitOn (pack "\n") instruction)

  print (takeFirstFromStack (orderByIndex(moveCrates crates instructions)) "")

buildIndexMap :: String -> [(Char, Int)]
buildIndexMap "" = []
buildIndexMap a = buildIndexMapPart a 0
  where
    buildIndexMapPart "" _ = []
    buildIndexMapPart (x : xs) i = if x == ' ' then buildIndexMapPart xs (i + 1) else (x, i) : buildIndexMapPart xs (i + 1)

convertCratesText :: [(Char, Int)] -> [String] -> [(Char, String)]
convertCratesText _ [] = []
convertCratesText [] _ = []
convertCratesText (x : xs) row = (grabCharByIndex x row) : convertCratesText xs row
  where
    grabCharByIndex :: (Char, Int) -> [String] -> (Char, String)
    grabCharByIndex _ [] = (' ', [])
    grabCharByIndex (col, index) (x : xs) = (col, removeElement ' ' ((x !! index) : snd (grabCharByIndex (col, index) xs)))

removeElement :: Char -> String -> String
removeElement _ "" = ""
removeElement a (x : xs) = if x == a then removeElement a xs else x : removeElement a xs

parseInstruction :: Text -> (Int, Char, Char)
parseInstruction a = do
  let a1 = replace (pack "move ") (pack "") a
  let a2 = replace (pack " from ") (pack ",") a1
  let a3 = replace (pack " to ") (pack ",") a2

  let datas = splitOn (pack ",") a3
  -- let num = fromIntegral (fst (decimal (datas!!0)))
  let num = fst (fromRight (0, pack "") (decimal (head datas)))
  let from = head (unpack (datas !! 1))
  let to = head (unpack (datas !! 2))
  (num, from, to)

moveCrates :: [(Char, String)] -> [(Int, Char, Char)] -> [(Char, String)]
moveCrates crates [] = crates
moveCrates crates (x : xs) = moveCrates (moveCratesPart crates x) xs
  where
    moveCratesPart :: [(Char, String)] -> (Int, Char, Char) -> [(Char, String)]
    moveCratesPart crates (num, from, to) = do
      let fromStack = snd (head (filter (\(x, _) -> x == from) crates))
      let toStack = snd (head (filter (\(x, _) -> x == to) crates))
      let newFromStack = drop num fromStack
      let newToStack = (take num fromStack) ++ toStack
      let newCrates = filter (\(x, _) -> x /= from) crates ++ [(from, newFromStack)]
      filter (\(x, _) -> x /= to) newCrates ++ [(to, newToStack)]

takeFirstFromStack :: [(Char, String)] -> String -> String
takeFirstFromStack [] a = a
takeFirstFromStack ((_, []) : _) _ = ""
takeFirstFromStack ((index, y : ys) : xs) a = takeFirstFromStack xs [y] ++ a

orderByIndex :: [(Char, String)] -> [(Char, String)]
orderByIndex [] = []
orderByIndex (x : xs) = orderByIndex (filter (\(a, _) -> a > fst x) xs) ++ [x] ++ orderByIndex (filter (\(a, _) -> a < fst x) xs)