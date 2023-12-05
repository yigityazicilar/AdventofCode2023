module AoC where

getFileContents :: String -> IO [String]
getFileContents path = do file <- readFile path
                          return $ lines file

splitString :: (Char -> Bool) -> String -> [String]
splitString f s = case dropWhile f s of
                    "" -> []
                    s' -> w : splitString f s''
                          where (w, s'') = break f s'

