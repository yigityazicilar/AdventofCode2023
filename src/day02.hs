import Data.Char (digitToInt, isSpace)
import qualified Data.Map as Map
import Data.Map (Map, elems)
import AoC (getFileContents, splitString)

main :: IO ()
main = do fileLines <- getFileContents "../resources/Day2/day2.txt"
          putStr "Solution to Day 2 Part 1 is "
          print $ parseFileP1 fileLines
          putStr "Solution to Day 2 Part 2 is "
          print $ parseFileP2 fileLines

parseFileP1 :: [String] -> Int
parseFileP1 [] = 0
                     -- [["5 green", "7 red"], ["8 blue"]] -> [[True, True], [True]] -> [True, True] -> True
parseFileP1 (x:xs) = if and $ map (and . map evalCubes) rounds then id + parseFileP1 xs else parseFileP1 xs
                     where line = splitString (==':') x
                           id = read $ last $ splitString (==' ') (head line)
                           -- rounds: [["5 green", "7 red"], ["8 blue"]]
                           rounds = map (map (dropWhile isSpace) . splitString (==',')) (splitString (==';') (last line))

evalCubes :: String -> Bool
evalCubes xs
            | colour == "red" = num <= 12
            | colour == "green" = num <= 13
            | colour == "blue" = num <= 14
            | otherwise = False
            where split = splitString (==' ') xs
                  num = read (head split) :: Int
                  colour = last split


parseFileP2 :: [String] -> Int
parseFileP2 [] = 0
                     -- ["5 green", "7 red", "8 blue", "6 red"] -> [("red", [7, 6]), ("green", [5]), ("blue", [8])] -> [[7, 6], [5], [8]] -> [7, 5, 8] -> 280 
parseFileP2 (x:xs) = product (map maximum (elems $ minimumPossible rounds)) + parseFileP2 xs
                     where line = splitString (==':') x
                           -- rounds = ["5 green", "7 red", "8 blue", "6 red"]
                           rounds = concatMap (map (dropWhile isSpace) . splitString (==',')) (splitString (==';') (last line))

minimumPossible :: [String] -> Map String [Int]
minimumPossible [] = Map.fromList [("red", []), ("green", []), ("blue", [])]
minimumPossible (x:xs) = Map.insertWith (++) colour [num] (minimumPossible xs)
                     where split = splitString (==' ') x
                           num = read (head split) :: Int
                           colour = last split



