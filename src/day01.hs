import Data.Char (isDigit, digitToInt)
import qualified Data.Map as Map
import Data.Map (filterWithKey, Map, elems)
import Data.List (isPrefixOf)
import AoC (getFileContents)

main = do fileLines <- getFileContents "../resources/Day1/day1.txt"
          putStr "Solution to Day 1 Part 1 is "
          print $ parseFile fileLines parseLineP1
          putStr "Solution to Day 1 Part 2 is "
          print $ parseFile fileLines parseLineP2


parseFile ::  [String] -> (String -> [Int]) -> Int
parseFile [] _ = 0
parseFile (x:xs) parseLine = head line * 10 + last line + parseFile xs parseLine
                   where line = parseLine x

parseLineP1 :: String -> [Int]
parseLineP1 line = map digitToInt $ filter (`elem` ['0'..'9']) line

parseLineP2 :: String -> [Int]
parseLineP2 [] = []
parseLineP2 xs
            | null match = if isDigit (head xs) then digitToInt (head xs) : recursion else recursion
            | otherwise = head (elems match) : recursion
            where match = filterWithKey (\h _ -> h `isPrefixOf` take 5 xs) numMap
                  recursion = parseLineP2 (tail xs)

numMap :: Map String Int
numMap = Map.fromList [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]