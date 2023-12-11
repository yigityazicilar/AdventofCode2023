import AoC (getFileContents, splitString)

main :: IO ()
main = do fileLines <- getFileContents "../resources/Day06/input.txt"
          let time = map read (splitString (==' ') $ dropWhile (`notElem` ['0'..'9']) $ head fileLines) :: [Double]
          let distance = map read $ splitString (==' ') $ dropWhile (`notElem` ['0'..'9']) $ last fileLines :: [Double]
          let mergedTime = read $ filter (`elem` ['0'..'9']) $ head fileLines :: Double
          let mergedDist = read $ filter (`elem` ['0'..'9']) $ last fileLines :: Double
          print $ product $ map (\(x, y) -> ceiling x - floor y - 1) $ zipWith quadraticFormula (map ((-1)*) time) distance
          print $ (\(x, y) -> ceiling x - floor y - 1) $ quadraticFormula (-mergedTime) mergedDist

quadraticFormula :: Floating b => b -> b -> (b, b)
quadraticFormula b c = ((-b + sqrt (b * b - 4 * c)) / 2, (-b - sqrt (b * b - 4 * c)) / 2)