import AoC (getFileContents, enumerate, insertWithList)
import Data.List (intersect)
import Data.Map (Map, elems, fromList)
import qualified Data.Bifunctor

main :: IO()
main = do fileLines <- getFileContents "../resources/Day3/day3.txt"
          let lineLen = length $ head fileLines
          let nums = concatMap (`getNums` lineLen) (enumerate fileLines)
          let symbols = getSymbolCoords (getSymbolIndices (\x -> x `notElem` (['0'..'9'] ++ ['.']))) fileLines
          let stars = getSymbolCoords (getSymbolIndices (=='*')) fileLines
          putStr "Solution to Day 3 Part 1 is "
          print $ getParts nums symbols
          putStr "Solution to Day 3 Part 2 is "
          print $ sum [product x | x <- elems (getGears nums stars), length x == 2]

------- COMMON -------

getNums :: (Int, String) -> Int -> [(String, (Int, Int))]
getNums (_, []) _ = []
getNums (row, xs) lineLen
                | head xs `notElem` ['0'..'9'] = getNums (row, dropWhile (`notElem` ['0'..'9']) xs) lineLen
                | otherwise = (takeWhile (`elem` ['0'..'9']) xs, (row, lineLen - length xs)) : getNums (row, dropWhile (`elem` ['0'..'9']) xs) lineLen

addCoords :: (Int, Int) -> (Int, Int) -> (Int, Int)
addCoords (i, j) (k, l) = (i + k, j + l)

getSymbolCoords :: ((Int, String) -> [(Int, Int)]) -> [String] -> [(Int, Int)]
getSymbolCoords f fileLines = map (Data.Bifunctor.second (length (head fileLines) -)) (concatMap f (enumerate fileLines))

getSymbolIndices :: (Char -> Bool) -> (Int, String) -> [(Int, Int)]
getSymbolIndices _ (row, []) = []
getSymbolIndices f (row, x:xs) = if f x then (row, length (x:xs)) : getSymbolIndices f (row, xs) else getSymbolIndices f (row, xs)

-------------P1--------------

getParts :: [(String, (Int, Int))] -> [(Int, Int)] -> Int
getParts [] _ = 0
getParts ((numStr, coords):xs) symbols
                                    | not (null matchedIndices) = read numStr + recursionRes
                                    | otherwise = recursionRes
                                        where possibleVals = [(x, y) | x <- [-1..1], y <- [-1..length numStr]]
                                              matchedIndices = map (addCoords coords) possibleVals `intersect` symbols
                                              recursionRes = getParts xs symbols

-------------P2--------------

getGears :: [(String, (Int, Int))] -> [(Int, Int)] -> Map (Int, Int) [Int]
getGears [] stars = fromList [(x, []) | x <- stars]
getGears ((numStr, coords):xs) stars
                                    | not (null matchedIndices) = insertWithList (++) matchedIndices (read numStr) recursionRes 
                                    | otherwise = recursionRes
                                      where possibleVals = [(x, y) | x <- [-1..1], y <- [-1..length numStr]]
                                            matchedIndices = map (addCoords coords) possibleVals `intersect` stars
                                            recursionRes = getGears xs stars
