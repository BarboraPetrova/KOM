{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Data.List (sort, elemIndex, mapAccumL)
import Data.Maybe(fromJust)
import qualified Data.Map as Map
import qualified Data.Vector as V

import System.Environment (getArgs)
import qualified Data.ByteString as BS
import Data.Word (Word8)

-----------------------------------------------
-- BWT (Burrows-Wheeler Transform)

rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]
rotate [] = []

-- builds all rotations of the input string and sorts them
createTable :: Ord a => [a] -> [[a]]
createTable str = sort $ take (length str) (iterate rotate str)

-- Burrows-Wheeler Transform
bwt :: Ord a => [a] -> ([a], Int)
bwt str = (lastCol, index)
    where
        table = createTable str
        lastCol = map last table
        index = fromJust $ elemIndex str table

-----------------------------------------------
-- Inverse BWT helper structures

-- maps each symbol to the list of its positions in sorted order
aMap :: Ord a => [a] -> Map.Map a (V.Vector Int)
aMap xs = 
    Map.map V.fromList $
    Map.map reverse $ 
    Map.fromListWith (++) [(x, [i]) | (i, x) <- zip [0..] xs]

-- assigns occurrence index to each element in the column
idxVec :: Ord a => [a] -> V.Vector (a, Int)
idxVec xs = V.fromList $ reverse $ snd $ foldl occCount (Map.empty, []) xs

-- counts occurrences of each symbol (for distinguishing duplicates)
occCount :: (Num b, Ord a) => (Map.Map a b, [(a, b)]) -> a -> (Map.Map a b, [(a, b)])
occCount (counts, acc) x = (counts', (x, ordOfApp) : acc)
  where
    ordOfApp = Map.findWithDefault 0 x counts + 1
    counts' = Map.insert x ordOfApp counts

-- retrieves (symbol, occurrence index) at a given position
aOcc :: Int -> V.Vector (a, Int) -> (a, Int)
aOcc idx vec = vec V.! idx

-- finds matching position in the first column
findSamePos :: Ord a => (a, Int) -> Map.Map a (V.Vector Int) -> Int
findSamePos (el, appr) mp = (mp Map.! el) V.! (appr - 1)

-- convert list to vector 
buildVec :: [a] -> V.Vector a
buildVec = V.fromList

-- build helper structures used for LF-mapping
buildMap :: Ord a => [a] -> Map.Map a (V.Vector Int)
buildMap xs = aMap (sort xs)

buildIdxVec :: Ord a => [a] -> V.Vector (a, Int)
buildIdxVec = idxVec

-- LF-mapping step function - moves from last column to first column
lfStep :: Ord a => V.Vector (a, Int) -> Map.Map a (V.Vector Int) -> Int -> Int
lfStep frstCol lastCol idx = findSamePos (aOcc idx frstCol) lastCol

-- generate full index chain
buildIdxChain :: Ord a => V.Vector (a, Int) -> Map.Map a (V.Vector Int) -> Int -> [Int]
buildIdxChain frstCol lastCol startIdx =
    reverse $
    take (V.length frstCol) $
    iterate (lfStep frstCol lastCol) startIdx

-- Inverse BWT: reconstructs original input from last column + index
ibwt :: Ord a => ([a], Int) -> [a]
ibwt (lastCol, startIdx) = map (buildVec lastCol V.!) indices
  where indices = buildIdxChain (buildIdxVec lastCol) (buildMap lastCol) startIdx

-----------------------------------------------
-- MTF (Move-To-Front transform)

type Alphabet = V.Vector Word8

-- initial fixed alphabet of all byte values
initAlphabet :: Alphabet
initAlphabet = V.fromList [0..255]

-- performs one MTF step: returns updated alphabet and index of the symbol
mtfStep :: Alphabet -> Word8 -> (Alphabet, Int)
mtfStep alpha x = (newAlpha, idx)
    where
        idx = fromJust $ V.elemIndex x alpha
        newAlpha = V.cons x (V.take idx alpha V.++ V.drop (idx + 1) alpha)

-- applies MTF to the whole input
mtfA :: [Word8] -> (Alphabet, [Int])
mtfA = mapAccumL mtfStep initAlphabet

-----------------------------------------------
-- Inverse MTF

imtfsStep :: Alphabet -> Int -> (Alphabet, Word8)
imtfsStep alpha idx = (newAlpha, x)
    where 
        x = alpha V.! idx
        newAlpha = V.cons x (V.take idx alpha V.++ V.drop (idx + 1) alpha)

imtfs :: [Int] -> [Word8]
imtfs xs = snd $ mapAccumL imtfsStep initAlphabet xs


-----------------------------------------------
-- Run-Length Encoding (RLE)
--
-- Compresses consecutive equal values into (value, count)

run :: Eq a => a -> Int -> [a] -> [(a, Int)]
run el count [] = [(el, count)]
run el count (x:xs)
  | el == x  = run el (count + 1) xs
  | otherwise = (el, count) : run x 1 xs

-- main RLE function
rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (x:xs) = run x 1 xs

expand :: (a, Int) -> [a]
expand (x, n) = replicate n x

-- decodes RLE back to original list
rleDecode :: [(a, Int)] -> [a]
rleDecode = concatMap expand


-----------------------------------------------
-- PIPELINE

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

-----------------------------------------------
-- MAIN 

main :: IO ()
main = do
    args <- getArgs

    input :: [Word8] <- case args of
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