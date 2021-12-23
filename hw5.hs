import Data.List (group)
import Data.Char (intToDigit)
toSeries :: String -> [String]
toSeries = group

encodeSeries :: [String] -> [(Int, Char)]
encodeSeries = map (\ x -> (length x, head x))

countcode :: [(Int, Char)] -> [[Char]]
countcode [] = []
countcode ((a,b):xs) = [intToDigit a,b]:countcode xs
writeCode :: [(Int, Char)] -> [Char]
writeCode [] =[]
writeCode xs = concat (countcode xs)