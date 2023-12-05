import AoC (getFileContents, splitString, enumerate)
import Data.Char (isSpace)
import Data.List (intersect)

main :: IO ()
main = do fileLines <- getFileContents "../resources/Day04/input.txt"
          let drawnNumbers = map (map read . splitString (==' ') . last . splitString (=='|')) fileLines :: [[Int]]
          let cardNumbers = map (map read . splitString (==' ') . last . splitString (==':') . head . splitString (=='|')) fileLines :: [[Int]]
          let matchCount = map length $ zipWith intersect cardNumbers drawnNumbers
          putStr "Solution to Day 4 Part 1 is "
          print $ sum [2^(x-1) | x <- matchCount, x > 0]
          putStr "Solution to Day 4 Part 2 is "
          print $ sum $ scratchcardWins matchCount (replicate (length matchCount) 1)

scratchcardWins :: [Int] -> [Int] -> [Int]
scratchcardWins [] [] = []
scratchcardWins (matchCount:xs) (currCount:ys) = currCount : scratchcardWins xs (map (currCount +) (take matchCount ys) ++ drop matchCount ys)