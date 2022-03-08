import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Char8 as C8 (pack)
import Data.List (group)
import Data.Char (intToDigit, chr)
import qualified Data.String as BS
import qualified Data.Word as GHC.Word
import System.Environment (getArgs)

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

main :: IO ()
main = do
    [mode,input,output] <- getArgs
    let filename = "test"
    let fileout = "testout"
    
    bytestr <- BS.readFile filename
    let strc = BS.unpack bytestr
    let series = toSeries strc
    let rlestr = encodeSeries series
    let rlestr1 = writeCode rlestr
    let rlebyte = BS.pack rlestr1
    BS.writeFile fileout rlebyte

    