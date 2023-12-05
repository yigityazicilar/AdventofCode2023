import AoC (getFileContents)

main :: IO()
main = do fileLines <- getFileContents "../resources/Day05/sample.txt"
          print fileLines