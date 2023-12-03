main :: IO ()
main = do
  input <- readFile "input"
  let mapElements = parseFile 0 0 input
  partOne mapElements
  partTwo mapElements


partOne :: [MapElement] -> IO ()
partOne mapElements = do
  let adjacentNumElements = filter (hasAdjacentSymbol mapElements) mapElements
  print $ sumElements adjacentNumElements

partTwo :: [MapElement] -> IO ()
partTwo mapElements = do
  let gears = filter isSymbolGear mapElements
  let gearsValue = map (gearValue mapElements) gears
  print $ sum gearsValue

data MapElement = Symbol Int Int Char | Num Int Int String deriving(Show)

isSymbolGear :: MapElement -> Bool
isSymbolGear (Symbol _ _ '*') = True
isSymbolGear _ = False

isSymbol :: MapElement -> Bool
isSymbol (Symbol _ _ _) = True
isSymbol _ = False

getNumberValue :: MapElement -> Int
getNumberValue (Num _ _ str) = read str
getNumberValue _ = 0

gearValue :: [MapElement] -> MapElement -> Int
gearValue mapElements (Symbol x y '*') =
  let
    adjacentNumbers = filter (isAdjacentNumber x y) mapElements
    adjacentNumbersInt = map getNumberValue adjacentNumbers
  in
    case adjacentNumbersInt of
      [] -> 0
      [_] -> 0
      [a,b] -> a * b
      _ -> 0
gearValue _ _ = 0

isAdjacentNumber :: Int -> Int -> MapElement -> Bool
isAdjacentNumber x y (Num nx ny str) =
  (y == ny && (nx == (x + 1) || (nx + length str) == x)) ||
  ((y - 1) == ny && (nx + length str) >= x && nx <= (x + 1)) ||
  ((y + 1) == ny && (nx + length str) >= x && nx <= (x + 1))
isAdjacentNumber _ _ _ = False

hasAdjacentSymbol :: [MapElement] -> MapElement -> Bool
hasAdjacentSymbol mapElements (Num x y str) =
  let
    symbolElements = filter isSymbol mapElements 
    top = any (inRangeSymbol x (y - 1) str)  symbolElements
    middle = any (inRangeSymbol x y str) symbolElements
    bottom = any (inRangeSymbol x (y + 1) str) symbolElements
  in
    top || middle || bottom
hasAdjacentSymbol _ (Symbol _ _ _) = False

inRangeSymbol :: Int -> Int -> String -> MapElement -> Bool
inRangeSymbol x y str (Symbol sx sy _) = y == sy && sx >= (x - 1) && sx <= (x + (length str))
inRangeSymbol _ _ _ _ = False

sumElements :: [MapElement] -> Int
sumElements [] = 0
sumElements (x:xs) =
  case x of
    (Num _ _ str) -> read str + sumElements xs
    (Symbol _ _ _) -> sumElements xs

parseFile :: Int -> Int -> String -> [MapElement]
parseFile _ _ [] = []
parseFile x y ('.':xs) = parseFile (x + 1) y xs
parseFile x y ('\n':xs) = parseFile 0 (y + 1) xs
parseFile x y (char:xs) =
  if isNumber char then
    parseFileNum x y (x + 1) y [char] xs
  else
    (Symbol x y char):(parseFile (x + 1) y xs)

parseFileNum :: Int -> Int -> Int -> Int -> String -> String -> [MapElement]
parseFileNum origX origY x y num (char:xs) =
  if isNumber char then
    parseFileNum origX origY (x + 1) y (char:num) xs
  else
    (Num origX origY (reverse num)):(parseFile x y (char:xs))


isNumber :: Char -> Bool
isNumber '0' = True
isNumber '1' = True
isNumber '2' = True
isNumber '3' = True
isNumber '4' = True
isNumber '5' = True
isNumber '6' = True
isNumber '7' = True
isNumber '8' = True
isNumber '9' = True
isNumber _ = False

