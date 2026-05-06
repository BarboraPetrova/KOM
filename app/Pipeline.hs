module Pipeline (bwtEncode, bwtDecode, toString) where

import Data.Word (Word8)

import Algo.BWT
import Algo.MTF
import Algo.RLE

-----------------------------------------------
-- PIPELINE
-------------------------------------------------

-- COMPRESSION:
-- Word8 → BWT → MTF → RLE

bwtEncode :: [Word8] -> ([(Int, Int)], Int)
bwtEncode xs = (rle indices, idx)
 where
    (bw, idx) = bwt xs
    (_, indices) = mtfA bw

-- DECOMPRESSION:
-- RLE → inverse MTF → inverse BWT

bwtDecode :: ([(Int, Int)], Int) -> [Word8]
bwtDecode (rleData, idx) = ibwt (imtfs $ rleDecode rleData, idx)


-- helper: convert bytes back to string
toString :: [Word8] -> String
toString = map (toEnum . fromIntegral)