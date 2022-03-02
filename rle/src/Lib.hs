module Lib
    ( toSeries,
      encodeSeries,
      countcode,
      writeCode
    ) where
import qualified Data.Word as GHC.Word
import Data.List ( group )

toSeries :: [GHC.Word.Word8] -> [[GHC.Word.Word8]]
toSeries = group

encodeSeries :: [[b]] -> [(Int, b)]
encodeSeries = map (\ x -> (length x, head x))


countcode :: (Integral a1, Num a2) => [(a1, a2)] -> [[a2]]
countcode [] = []
countcode ((a,b):xs) =[fromIntegral a,b]:countcode xs

writeCode :: (Integral a1, Num a2) => [(a1, a2)] -> [a2]
writeCode [] =[]
writeCode xs = concat (countcode xs)
