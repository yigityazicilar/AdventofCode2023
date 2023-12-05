module AoC where
import Data.Map ( Map, insertWith )

getFileContents :: String -> IO [String]
getFileContents path = do file <- readFile path
                          return $ lines file

splitString :: (Char -> Bool) -> String -> [String]
splitString f s = case dropWhile f s of
                    "" -> []
                    s' -> w : splitString f s''
                          where (w, s'') = break f s'

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

insertWithList :: (Ord k) => (a -> a -> a) -> [k] -> a -> Map k a -> Map k a
insertWithList _ [] _ m = m
insertWithList f (x:xs) num m = insertWith f x num (insertWithList f xs num m)

