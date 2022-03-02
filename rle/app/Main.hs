module Main where
import qualified Data.ByteString as BS
import qualified Lib as Enc

main :: IO ()
main = do
    let filename = "text.txt"
    bytestr <- BS.readFile filename
    let strc = BS.unpack bytestr
    let series = Enc.toSeries strc
    let rlestr = Enc.encodeSeries series
    let rlestr1 = Enc.writeCode rlestr
    let rlebyte = BS.pack rlestr1
    BS.writeFile filename rlebyte

