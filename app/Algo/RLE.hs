module Algo.RLE (rle, rleDecode) where

-------------------------------------------------
-- Run-Length Encoding (RLE)
-------------------------------------------------
-- Core idea:
-- Compress sequences of repeated values by replacing
-- consecutive duplicates with (value, count).

-------------------------------------------------
-- Internal helper: builds one run of equal elements
run :: Eq a => a -> Int -> [a] -> [(a, Int)]
run el count [] = [(el, count)]
run el count (x:xs)
  | el == x  = run el (count + 1) xs
  | otherwise = (el, count) : run x 1 xs

{-
Example
>>> run 'a' 1 "aabbc"
[('a',3),('b',2),('c',1)]
-}

-------------------------------------------------
-- Main encoding function

-- Handles empty input and starts first run
rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (x:xs) = run x 1 xs

{-
Examples:

>>> rle "aaabbc"
[('a',3),('b',2),('c',1)]

>>> rle "abcd"
[('a',1),('b',1),('c',1),('d',1)]

>>> rle ""
[]

>>> rle [1,1,1,2,2,3]
[(1,3),(2,2),(3,1)]
-}

-------------------------------------------------
-- Decoding helper

-- Expands a single (value, count) pair back into a list
expand :: (a, Int) -> [a]
expand (x, n) = replicate n x

{-
Examples:

>>> expand ('a',3)
"aaa"

>>> expand (5,4)
[5,5,5,5]
-}


-------------------------------------------------
-- RLE decoding

-- Restores original list by expanding all runs
rleDecode :: [(a, Int)] -> [a]
rleDecode = concatMap expand

{-
Examples:

>>> rleDecode [('a',3),('b',2),('c',1)]
"aaabbc"

>>> rleDecode [(1,3),(2,2),(3,1)]
[1,1,1,2,2,3]

>>> rleDecode []
[]

-------------------------------------------------
Round-trip property:

>>> rleDecode (rle "aaabbc")
"aaabbc"

>>> rle (rleDecode [(1,3),(2,2)])
[(1,3),(2,2)]
-------------------------------------------------
-}