module Algo.MTF (Alphabet, initAlphabet, mtfA, imtfs) where

import Data.Word (Word8)
import qualified Data.Vector as V
import Data.List (mapAccumL)
import Data.Maybe (fromJust)

-------------------------------------------------
-- Move-To-Front Transform (MTF)
-------------------------------------------------
-- Core idea:
-- Maintain a list (alphabet) of symbols.
-- For each input symbol:
--   1. output its index in the alphabet
--   2. move it to the front
--
-- Frequently repeated symbols get small indices,
-- which makes data more compressible.

-------------------------------------------------
-- Alphabet representation

-- The alphabet is a vector of all possible byte values (0–255)
-- We use Vector for efficient indexing and slicing.
type Alphabet = V.Vector Word8

-- Initial fixed alphabet of all byte values
initAlphabet :: Alphabet
initAlphabet = V.fromList [0..255]


-------------------------------------------------
-- Single MTF encoding step

-- Given current alphabet and a symbol:
--   - find its index
--   - move it to the front
--   - return updated alphabet + index
mtfStep :: Alphabet -> Word8 -> (Alphabet, Int)
mtfStep alpha x = (newAlpha, idx)
    where
        -- position of symbol in current alphabet
        idx = fromJust $ V.elemIndex x alpha

        -- rebuild alphabet with x moved to front
        -- structure: [x] ++ elements before x ++ elements after x
        newAlpha = V.cons x (V.take idx alpha V.++ V.drop (idx + 1) alpha)

-------------------------------------------------
-- Full MTF encoding

-- Applies MTF to the whole input
mtfA :: [Word8] -> (Alphabet, [Int])
mtfA = mapAccumL mtfStep initAlphabet

-------------------------------------------------
-- Inverse MTF
-------------------------------------------------
-- Core idea:
-- We reverse the process:
--   1. use index to get symbol
--   2. move that symbol to front
--   3. continue updating alphabet

-------------------------------------------------
-- Single decoding step
imtfsStep :: Alphabet -> Int -> (Alphabet, Word8)
imtfsStep alpha idx = (newAlpha, x)
    where 
        x = alpha V.! idx
        newAlpha = V.cons x (V.take idx alpha V.++ V.drop (idx + 1) alpha)

-------------------------------------------------
-- Full inverse transform

-- Reconstruct original byte stream from MTF indices
imtfs :: [Int] -> [Word8]
imtfs xs = snd $ mapAccumL imtfsStep initAlphabet xs

