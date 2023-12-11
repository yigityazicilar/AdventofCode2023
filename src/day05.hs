import AoC (getFileContents, splitString)
import Debug.Trace (trace)
import Data.Set (Set, union, fromList)

main :: IO()
main = do emptyLineFileLines <- getFileContents "../resources/Day05/input.txt"
          let fileLines = filter (/="") emptyLineFileLines
          let seeds = map read $ splitString (==' ') $ last $ splitString (==':') (head fileLines) :: [Int]
          let maps = map (map (map read . splitString (==' '))) (getMappings (tail fileLines)) :: [[[Int]]]
          putStr "Solution to Day 5 Part 1 is "
          print $ minimum $ map (`getLocation` maps) seeds
          putStr "Solution to Day 5 Part 2 is "

getMappings :: [String] -> [[String]]
getMappings [] = []
getMappings xs
                | ':' == last (head xs) = getMappings (tail xs)
                | otherwise = takeWhile (\x -> head x `elem` ['0'..'9']) xs : getMappings (dropWhile (\x -> head x `elem` ['0'..'9']) xs)


getLocation :: Int -> [[[Int]]] -> Int
getLocation seed [x] = applyMapForward seed x
getLocation seed (x:xs) = getLocation (applyMapForward seed x) xs

getSeed :: Int -> [[[Int]]] -> Int
getSeed loc [x] = applyMapBackward loc x
getSeed loc (x:xs) = getSeed (applyMapBackward loc x) xs

applyMapForward :: Int -> [[Int]] -> Int
applyMapForward seed [] = seed
applyMapForward seed ([dest, start, range]:xs)
                                    | (seed >= start) && (seed <= start + (range - 1)) = dest + (seed - start)
                                    | otherwise = applyMapForward seed xs

applyMapBackward :: Int -> [[Int]] -> Int
applyMapBackward seed [] = seed
applyMapBackward seed ([dest, start, range]:xs)
                                        | (seed >= dest) && (seed <= dest + (range - 1)) = start + (seed - dest)
                                        | otherwise = applyMapBackward seed xs

searchSeed :: [[[Int]]] -> [(Int, Int)] -> Int -> Int
searchSeed maps xs loc
                    | any (\(x, y) -> seed >= x && seed <= y) xs = loc
                    | otherwise = searchSeed maps xs (loc + 1)
                      where seed = getSeed loc maps

seedRange :: [Int] -> [(Int, Int)]
seedRange [] = []
seedRange (x:y:rest) = (x, x+y-1) : seedRange rest
