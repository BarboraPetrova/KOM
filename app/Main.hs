module Main where

import System.Environment (getArgs)
import qualified Data.ByteString as BS
import Data.Word (Word8)

import Pipeline (bwtEncode, bwtDecode, toString)
-----------------------------------------------
-- MAIN 

main :: IO ()
main = do
    args <- getArgs

    input <- case args of
        [] -> map (fromIntegral . fromEnum) <$> getContents
        (file:_) -> BS.unpack <$> BS.readFile file

    -- compression
    let (encoded, idx) = bwtEncode input

    putStrLn "Encoded:"
    print encoded
    print idx

    -- decompression
    let decoded = toString $ bwtDecode (encoded, idx)

    putStrLn "Decoded:"
    print decoded