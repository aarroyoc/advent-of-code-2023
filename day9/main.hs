main :: IO ()
main = do
  input <- readFile "input"
  let listOfNums = map words (lines input)
  let sol1 = sum $ map extrapolate (map (map read) listOfNums)
  let sol2 = sum $ map extrapolateBack (map (map read) listOfNums)
  print sol1
  print sol2

extrapolate :: [Int] -> Int
extrapolate nums =
  let
    dl = diffList nums
  in
    if zeroList dl then
      last nums
    else
      (last nums) + (extrapolate dl)

extrapolateBack :: [Int] -> Int
extrapolateBack nums =
  let
    dl = diffList nums
  in
    if zeroList dl then
      head nums
    else
      (head nums) - (extrapolateBack dl)
    

diffList :: [Int] -> [Int]
diffList [a,b] = [b - a]
diffList (a:b:xs) = (b - a):(diffList (b:xs))

zeroList :: [Int] -> Bool
zeroList = all isZero
  where
    isZero a = a == 0 
