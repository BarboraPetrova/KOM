module Algo.BWT (bwt,ibwt) where

import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Vector as V

-------------------------------------------------
-- Burrows-Wheeler Transform (BWT)
-------------------------------------------------
-- Core idea:
-- 1. Generate all cyclic rotations of the input
-- 2. Sort them lexicographically
-- 3. Extract:
--      - last column (BWT result)
--      - index of original string in sorted table

-------------------------------------------------
-- Rotation helper

-- Rotate a list left by one position
rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]
rotate [] = []

{-
Example:
>>> rotate "abcd"
"bcda"

>>> rotate [1,2,3,4]
[2,3,4,1]
-}

-------------------------------------------------
-- Build rotation table

-- Generate all rotations and sort them
createTable :: Ord a => [a] -> [[a]]
createTable str = sort $ take (length str) (iterate rotate str)

{-
Example:
>>> createTable "abc"
["abc","bca","cab"]

-}

-------------------------------------------------
-- BWT encoding
bwt :: Ord a => [a] -> ([a], Int)
bwt str = (lastCol, index)
    where
        table = createTable str

        -- last column of sorted rotation matrix
        lastCol = map last table 

        -- position of original string in sorted rotation table
        index = fromJust $ elemIndex str table

{-
Example:
>>> bwt "pepapapapapu"
("pppppeaaauap",9)
-}

-------------------------------------------------
-- Inverse BWT
-------------------------------------------------
-- Core idea:
-- Use LF-mapping (Last-to-First mapping):
-- reconstruct original string by walking through permutations

-------------------------------------------------
-- Handling occurrence
-------------------------------------------------
-- Since characters can repeat, we annotate each occurrence
-- so every symbol is uniquely identifiable.

-------------------------------------------------
-- Map: symbol -> sorted row indices in first column

aMap :: Ord a => [a] -> Map.Map a (V.Vector Int)
aMap xs = 
    Map.map V.fromList $
    Map.map reverse $ 
    Map.fromListWith (++) [(x, [i]) | (i, x) <- zip [0..] xs]

{-
Example:
>>> aMap "banana"
fromList [('a',[1,3,5]),('b',[0]),('n',[2,4])]
-}

-------------------------------------------------
-- Annotate occurrences in sequence order

-- Converts string into (char, occurrence index) pairs
idxVec :: Ord a => [a] -> V.Vector (a, Int)
idxVec xs = V.fromList $ reverse $ snd $ foldl occCount (Map.empty, []) xs

-- Tracks how many times each character has appeared
occCount :: (Num b, Ord a) => (Map.Map a b, [(a, b)]) -> a -> (Map.Map a b, [(a, b)])
occCount (counts, acc) x = (counts', (x, ordOfOcc) : acc)
  where
    ordOfOcc = Map.findWithDefault 0 x counts + 1
    counts' = Map.insert x ordOfOcc counts

{-
Example:
>>> idxVec "banana"
[('b',1),('a',1),('n',1),('a',2),('n',2),('a',3)]
-}

-------------------------------------------------
-- Vector helper

-- Retrieve element at index
aOcc :: Int -> V.Vector (a, Int) -> (a, Int)
aOcc idx vec = vec V.! idx

-------------------------------------------------
-- LF-mapping support

-- Given (symbol, occurrence), find matching position in first column
findSamePos :: Ord a => (a, Int) -> Map.Map a (V.Vector Int) -> Int
findSamePos (el, appr) mp = (mp Map.! el) V.! (appr - 1)

-----------------------------------------------
-- Convert list to vector
buildVec :: [a] -> V.Vector a
buildVec = V.fromList

-------------------------------------------------
-- Build first column index structure (sorted input)
buildMap :: Ord a => [a] -> Map.Map a (V.Vector Int)
buildMap xs = aMap (sort xs)

-------------------------------------------------
-- LF-mapping step

-- Moves from last column position -> corresponding first column position
lfStep :: Ord a => V.Vector (a, Int) -> Map.Map a (V.Vector Int) -> Int -> Int
lfStep frstCol lastCol idx = findSamePos (aOcc idx frstCol) lastCol

-------------------------------------------------
-- Build full LF-mapping chain
buildIdxChain :: Ord a => V.Vector (a, Int) -> Map.Map a (V.Vector Int) -> Int -> [Int]
buildIdxChain frstCol lastCol startIdx =
    reverse $
    take (V.length frstCol) $
    iterate (lfStep frstCol lastCol) startIdx

-------------------------------------------------
-- Inverse BWT (decoding)
ibwt :: Ord a => ([a], Int) -> [a]
ibwt (lastCol, startIdx) = map (buildVec lastCol V.!) indices
  where indices = buildIdxChain (idxVec lastCol) (buildMap lastCol) startIdx

{-
Example:
>>> ibwt ("pppppeaaauap",9)
"pepapapapapu"
-}
