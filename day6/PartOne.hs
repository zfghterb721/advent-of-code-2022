import Data.List
main = do
  contents <- readFile "data.txt"
  print contents
  print (checkForMarker 14 contents 0)

checkForMarker :: Int -> String -> Int -> Int
checkForMarker 0 _ i = i
checkForMarker _ "" i = i
checkForMarker a (x:xs) i = if diversity (take a (x:xs)) then i+a else checkForMarker a xs i+1
  where
    diversity :: String -> Bool
    diversity j = length (nub j) == length j